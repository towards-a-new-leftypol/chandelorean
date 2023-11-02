BEGIN TRANSACTION;

-- <bktree/sql/init.sql>
-- CREATE EXTENSION bktree; -- only superuser can

-- Check whether any of our opclasses fail amvalidate
SELECT amname, opcname
FROM pg_opclass opc LEFT JOIN pg_am am ON am.oid = opcmethod
WHERE opc.oid >= 16384 AND NOT amvalidate(opc.oid);

-- </bktree/sql/init.sql>

DROP TABLE IF EXISTS sites CASCADE;
DROP TABLE IF EXISTS boards CASCADE;
DROP TABLE IF EXISTS threads CASCADE;
DROP TABLE IF EXISTS posts CASCADE;
DROP TABLE IF EXISTS attachments CASCADE;
DROP FUNCTION IF EXISTS update_post_body_search_index;
DROP FUNCTION IF EXISTS fetch_top_threads;
DROP FUNCTION IF EXISTS fetch_catalog;


-- It won't let us drop roles otherwise and the IFs are to keep this script idempotent.
DO
$$BEGIN
IF EXISTS (SELECT FROM pg_roles WHERE rolname = 'chan_archiver') THEN
    EXECUTE 'REVOKE ALL PRIVILEGES ON DATABASE chan_archives FROM chan_archiver';
END IF;
IF EXISTS (SELECT FROM pg_roles WHERE rolname = 'chan_archive_anon') THEN
    EXECUTE 'REVOKE ALL PRIVILEGES ON DATABASE chan_archives FROM chan_archive_anon';
END IF;
END$$;

DROP ROLE IF EXISTS chan_archiver;
DROP ROLE IF EXISTS chan_archive_anon;

CREATE TABLE IF NOT EXISTS sites
    ( site_id serial primary key
    , name text NOT NULL UNIQUE
    , url text NOT NULL
    );

CREATE TABLE IF NOT EXISTS boards
    ( board_id serial primary key
    , name text
    , pathpart text NOT NULL -- if it's /a/ then the pathpart is a
    , site_id int NOT NULL
    , CONSTRAINT site_fk FOREIGN KEY (site_id) REFERENCES sites (site_id) ON DELETE CASCADE
    , CONSTRAINT unique_site_board_id_constraint UNIQUE (site_id, pathpart)
    );

CREATE TABLE IF NOT EXISTS threads
    ( thread_id bigserial primary key
    , board_thread_id bigint NOT NULL -- this is the id of the thread in lainchan, mysql
    , creation_time timestamp with time zone NOT NULL
    , board_id int NOT NULL
    , CONSTRAINT board_fk FOREIGN KEY (board_id) REFERENCES boards (board_id) ON DELETE CASCADE
    , CONSTRAINT unique_board_board_thread_id_constraint UNIQUE (board_id, board_thread_id)
    );
CREATE INDEX threads_creation_time_idx   ON threads (creation_time);
CREATE INDEX threads_board_id_idx        ON threads (board_id);
CREATE INDEX threads_board_thread_id_idx ON threads (board_thread_id);

CREATE TABLE IF NOT EXISTS posts
    ( post_id bigserial primary key
    , board_post_id bigint NOT NULL
    , creation_time timestamp with time zone NOT NULL
    , body text
    , subject text
    , name text
    , email text
    , body_search_index tsvector
    , thread_id bigint NOT NULL
    , CONSTRAINT unique_thread_board_id_constraint UNIQUE (thread_id, board_post_id)
    , CONSTRAINT thread_fk FOREIGN KEY (thread_id) REFERENCES threads (thread_id) ON DELETE CASCADE
    );
CREATE INDEX posts_creation_time_idx ON posts (creation_time);
CREATE INDEX posts_body_search_idx   ON posts USING GIN (body_search_index);
CREATE INDEX posts_thread_id_idx     ON posts (thread_id);
CREATE INDEX posts_board_post_id_idx ON posts (board_post_id);
CREATE INDEX posts_thread_id_creation_time_idx ON posts (creation_time, thread_id);
--CREATE INDEX posts_thread_id_board_post_id_idx ON posts (thread_id, board_post_id);

CREATE OR REPLACE FUNCTION update_post_body_search_index() RETURNS trigger AS $$
BEGIN
    NEW.body_search_index :=
        (
            setweight(to_tsvector('english', COALESCE(NEW.subject, '')), 'A') ||
            setweight(to_tsvector('english', COALESCE(NEW.name, '')), 'B') ||
            setweight(to_tsvector('english', COALESCE(NEW.body, '')), 'C')
        );
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_update_post_body_search_index
BEFORE INSERT OR UPDATE
ON posts
FOR EACH ROW
EXECUTE FUNCTION update_post_body_search_index();

CREATE TABLE IF NOT EXISTS attachments
    ( attachment_id bigserial primary key
    , mimetype text NOT NULL
    , creation_time timestamp with time zone NOT NULL
    , md5_hash text NOT NULL
    , phash bigint
    , illegal boolean NOT NULL DEFAULT false
    , post_id bigint NOT NULL
    , CHECK
        (
            (mimetype NOT IN ('image/jpeg', 'image/png', 'image/gif'))
            OR
            (phash IS NOT NULL)
        )
    , CONSTRAINT post_fk FOREIGN KEY (post_id) REFERENCES posts (post_id) ON DELETE CASCADE
    );
CREATE INDEX attachments_creation_time_idx  ON attachments (creation_time);
CREATE INDEX attachments_post_id_idx        ON attachments (post_id);
CREATE INDEX attachments_md5_hash_idx       ON attachments (md5_hash);
CREATE INDEX attachments_phash_bktree_index ON attachments USING spgist (phash bktree_ops);


/*
 * Function Definitions
 */

/*
CREATE OR REPLACE FUNCTION insert_posts_and_return_ids(new_posts posts[])
RETURNS TABLE (post_id bigint, board_post_id bigint) AS $$
WITH inserted AS (
    INSERT INTO posts (board_post_id, creation_time, body, thread_id)
    SELECT np.board_post_id, np.creation_time, np.body, np.thread_id
    FROM unnest(new_posts) AS np
    ON CONFLICT (thread_id, board_post_id) DO NOTHING
    RETURNING post_id, board_post_id
),
selected AS (
    SELECT post_id, board_post_id
    FROM posts
    WHERE (thread_id, board_post_id) IN (SELECT thread_id, board_post_id FROM unnest(new_posts))
)
SELECT * FROM inserted
UNION ALL
SELECT * FROM selected WHERE (post_id, board_post_id) NOT IN (SELECT post_id, board_post_id FROM inserted);
$$ LANGUAGE sql;

-- 3m37s for clean db
-- 1m34s for full db (nothing inserted)

*/

CREATE OR REPLACE FUNCTION insert_posts_and_return_ids(new_posts posts[])
RETURNS TABLE (post_id bigint, board_post_id bigint, thread_id bigint) AS $$
WITH 
selected AS (
    SELECT post_id, board_post_id, thread_id
    FROM posts
    WHERE (thread_id, board_post_id) IN (SELECT thread_id, board_post_id FROM unnest(new_posts))
),
to_insert AS (
    SELECT np.*
    FROM unnest(new_posts) AS np
    LEFT OUTER JOIN selected s ON np.thread_id = s.thread_id AND np.board_post_id = s.board_post_id
    WHERE s.post_id IS NULL
),
inserted AS (
    INSERT INTO posts (board_post_id, creation_time, body, subject, name, email, thread_id)
    SELECT board_post_id, creation_time, body, subject, name, email, thread_id
    FROM to_insert
    RETURNING post_id, board_post_id, thread_id
)
SELECT * FROM inserted
UNION ALL
SELECT * FROM selected;
$$ LANGUAGE sql;

-- 1:51 for clean db (this varies a lot)
-- 1:21 for full db (nothing inserted)


CREATE OR REPLACE FUNCTION fetch_top_threads(
    p_start_time TIMESTAMPTZ,
    lookback INT DEFAULT 10000
)
RETURNS TABLE(bump_time TIMESTAMPTZ, post_count BIGINT, thread_id BIGINT, where_to_leave_off TIMESTAMPTZ)
LANGUAGE sql
AS $$
    SELECT 
        max(creation_time) as bump_time, 
        count(*),
        thread_id, 
        min(creation_time) as where_to_leave_off
    FROM
    (
        SELECT thread_id, creation_time
        FROM posts
        WHERE creation_time < p_start_time
        ORDER BY creation_time DESC 
        LIMIT LEAST(lookback, 250000)  -- capping the lookback to 250k
    ) as t
    GROUP BY thread_id
    ORDER BY bump_time DESC;
$$;


CREATE OR REPLACE FUNCTION fetch_catalog(max_time timestamptz, max_row_read int DEFAULT 10000)
RETURNS TABLE (
    post_count bigint,
    estimated_post_count bigint,
    post_id bigint,
    board_post_id bigint,
    creation_time timestamptz,
    bump_time timestamptz,
    body text,
    subject text,
    thread_id bigint,
    board_thread_id bigint,
    pathpart text,
    name text
) AS $$
    WITH
        top AS
        (
            SELECT * FROM fetch_top_threads(max_time, max_row_read) AS top
        ),
        tall_posts AS
        (
            SELECT
                top.post_count as estimated_post_count,
                posts.post_id,
                posts.board_post_id,
                posts.creation_time,
                top.bump_time,
                posts.body,
                posts.subject,
                posts.thread_id
            FROM top
            JOIN posts ON top.thread_id = posts.thread_id
            WHERE creation_time < max_time
        ),
        op_posts AS
        (
            SELECT DISTINCT ON (t.thread_id)
                *
            FROM tall_posts t
            ORDER BY t.thread_id, t.board_post_id
        ),
        post_counts AS
        (
            SELECT thread_id, count(*) as post_count FROM
            tall_posts
            GROUP BY thread_id
        )
    SELECT
        post_counts.post_count,
        op_posts.*,
        threads.board_thread_id,
        boards.pathpart,
        sites."name"
    FROM op_posts
    JOIN post_counts ON op_posts.thread_id = post_counts.thread_id
    JOIN threads ON op_posts.thread_id = threads.thread_id
    JOIN boards ON threads.board_id = boards.board_id
    JOIN sites ON sites.site_id = boards.site_id
    ORDER BY bump_time DESC;
$$ LANGUAGE sql;


/*
 * Permissions
 */

CREATE ROLE chan_archive_anon nologin;
GRANT CONNECT ON DATABASE chan_archives TO chan_archive_anon;
GRANT SELECT ON sites                   TO chan_archive_anon;
GRANT SELECT ON boards                  TO chan_archive_anon;
GRANT SELECT ON threads                 TO chan_archive_anon;
GRANT SELECT ON posts                   TO chan_archive_anon;
GRANT SELECT ON attachments             TO chan_archive_anon;
-- GRANT usage, select ON SEQUENCE sites_site_id_seq TO chan_archive_anon;
-- GRANT usage, select ON SEQUENCE boards_board_id_seq TO chan_archive_anon;
GRANT chan_archive_anon                 TO admin;

CREATE ROLE chan_archiver noinherit login password 'test_password';
GRANT CONNECT ON DATABASE chan_archives TO chan_archiver;
GRANT chan_archive_anon                 TO chan_archiver;
GRANT ALL ON sites                      TO chan_archiver;
GRANT ALL ON boards                     TO chan_archiver;
GRANT ALL ON threads                    TO chan_archiver;
GRANT ALL ON posts                      TO chan_archiver;
GRANT ALL ON attachments                TO chan_archiver;
GRANT EXECUTE ON FUNCTION update_post_body_search_index TO chan_archiver;
GRANT EXECUTE ON FUNCTION insert_posts_and_return_ids   TO chan_archiver;
GRANT EXECUTE ON FUNCTION fetch_top_threads             TO chan_archiver;
GRANT EXECUTE ON FUNCTION fetch_catalog                 TO chan_archiver;
GRANT usage, select ON SEQUENCE sites_site_id_seq       TO chan_archiver;
GRANT usage, select ON SEQUENCE boards_board_id_seq     TO chan_archiver;
GRANT usage, select ON SEQUENCE threads_thread_id_seq   TO chan_archiver;
GRANT usage, select ON SEQUENCE posts_post_id_seq       TO chan_archiver;

GRANT chan_archiver TO admin;

COMMIT;
