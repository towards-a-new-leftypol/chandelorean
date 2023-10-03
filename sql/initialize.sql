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
    , name text NOT NULL
    , url text NOT NULL
    );

CREATE TABLE IF NOT EXISTS boards
    ( board_id serial primary key
    , name text NOT NULL
    , pathpart text NOT NULL -- if it's /a/ then the pathpart is a
    , site_id int NOT NULL
    , CONSTRAINT site_fk FOREIGN KEY (site_id) REFERENCES sites (site_id) ON DELETE CASCADE
    );

CREATE TABLE IF NOT EXISTS threads
    ( thread_id bigserial primary key
    , board_thread_id bigint NOT NULL
    , creation_time timestamp with time zone NOT NULL
    , board_id int NOT NULL
    , CONSTRAINT board_fk FOREIGN KEY (board_id) REFERENCES boards (board_id) ON DELETE CASCADE
    , CONSTRAINT unique_board_board_thread_id_constraint UNIQUE (board_id, board_thread_id)
    );
CREATE INDEX threads_board_id_idx ON threads (board_id);

CREATE TABLE IF NOT EXISTS posts
    ( post_id bigserial primary key
    , board_post_id bigint NOT NULL
    , creation_time timestamp with time zone NOT NULL
    , body text
    , body_search_index tsvector
    , thread_id bigint NOT NULL
    , CONSTRAINT unique_thread_board_id_constraint UNIQUE (thread_id, board_post_id)
    , CONSTRAINT thread_fk FOREIGN KEY (thread_id) REFERENCES threads (thread_id) ON DELETE CASCADE
    );
CREATE INDEX posts_body_search_idx ON posts USING GIN (body_search_index);
CREATE INDEX posts_thread_id_idx ON posts (thread_id);
CREATE INDEX posts_board_post_id_idx ON posts (board_post_id);

CREATE OR REPLACE FUNCTION update_post_body_search_index() RETURNS trigger AS $$
BEGIN
    NEW.body_search_index := to_tsvector('english', NEW.body);
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
CREATE INDEX attachments_post_id_idx ON attachments (post_id);
CREATE INDEX attachments_md5_hash_idx ON attachments (md5_hash);
CREATE INDEX attachments_phash_bktree_index ON attachments USING spgist (phash bktree_ops);

CREATE ROLE chan_archive_anon nologin;
GRANT CONNECT ON DATABASE chan_archives TO chan_archive_anon;
GRANT SELECT ON sites                   TO chan_archive_anon;
GRANT SELECT ON boards                  TO chan_archive_anon;
GRANT SELECT ON threads                 TO chan_archive_anon;
GRANT SELECT ON posts                   TO chan_archive_anon;
GRANT SELECT ON attachments             TO chan_archive_anon;
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
GRANT chan_archiver TO admin;

COMMIT;
