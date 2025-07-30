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


SELECT * FROM fetch_top_threads(NOW(), 1000);

SELECT * FROM fetch_top_threads(NOW() - INTERVAL '10d') top JOIN posts ON top.thread_id = posts.thread_id;


SELECT
    ordered_posts.thread_id,
    count(*),
    array_agg(ordered_posts.body)
FROM (
    SELECT top.thread_id, posts.body
    FROM fetch_top_threads(NOW() - INTERVAL '10d', 2000) top
    JOIN posts ON top.thread_id = posts.thread_id
    ORDER BY posts.board_post_id ASC
) AS ordered_posts
GROUP BY ordered_posts.thread_id;

WITH
    top AS
        (
            SELECT * FROM fetch_top_threads(NOW()) top
        ),
	tall_posts AS
		(
			SELECT top.post_count as estimated_post_count, posts.*
			FROM top
			JOIN posts ON top.thread_id = posts.thread_id
		),
	op_posts AS
		(
            SELECT DISTINCT ON (t.thread_id)
                *
            FROM tall_posts t
            ORDER BY t.thread_id, t.board_post_id
        )
SELECT * FROM op_posts;


WITH
    top AS
        (
            SELECT * FROM fetch_top_threads(NOW()) top
        ),
	tall_posts AS
		(
			SELECT top.post_count as estimated_post_count, posts.*
			FROM top
			JOIN posts ON top.thread_id = posts.thread_id
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
SELECT post_counts.post_count, op_posts.* FROM op_posts JOIN post_counts ON op_posts.thread_id = post_counts.thread_id;


-- 71ms!
WITH
    top AS
        (
            SELECT * FROM fetch_top_threads(NOW() - INTERVAL '1y', 1000) top
        ),
	tall_posts AS
		(
			SELECT
				top.post_count as estimated_post_count,
				top.bump_time,
				posts.post_id,
				posts.board_post_id,
				posts.creation_time,
				posts.body,
				posts.thread_id
			FROM top
			JOIN posts ON top.thread_id = posts.thread_id
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
JOIN sites ON sites.site_id = boards.site_id;

DROP TYPE catalog_grid_result;
DROP FUNCTION search_posts;
DROP FUNCTION fetch_catalog;

CREATE TYPE catalog_grid_result AS
    (
        -- post_count bigint,
        estimated_post_count bigint,
        post_id bigint,
        board_post_id bigint,
        creation_time timestamptz,
        bump_time timestamptz,
        body text,
        subject text,
        thread_id bigint,
        embed text,
        board_thread_id bigint,
        pathpart text,
        site_name text,
        site_id int,
        file_mimetype text,
        file_illegal boolean,
        file_resolution dimension,
        file_name text,
        file_extension text,
        file_thumb_extension text
    );


CREATE OR REPLACE FUNCTION fetch_catalog(max_time timestamptz, max_row_read int DEFAULT 10000)
RETURNS SETOF catalog_grid_result AS $$
    WITH
        top AS
        (
            SELECT * FROM fetch_top_threads(max_time, max_row_read) AS top
        ),
        tall_posts AS
        (
            SELECT
                top.post_count AS estimated_post_count,
                posts.post_id,
                posts.board_post_id,
                posts.creation_time,
                top.bump_time,
                posts.body,
                posts.subject,
                posts.thread_id,
                posts.embed
            FROM top
            JOIN posts ON top.thread_id = posts.thread_id AND posts.local_idx = 1
            WHERE creation_time < max_time
        )
    SELECT
        -- post_counts.post_count,
        tall_posts.*,
        threads.board_thread_id, -- this should be part of the url path when creating links, not thread_id (that's internal)
        boards.pathpart,
        sites."name",
        sites.site_id,
        attachments.mimetype AS file_mimetype,
        attachments.illegal AS file_illegal,
        attachments.resolution AS file_resolution,
        attachments.board_filename AS file_name,
        attachments.file_extension,
        attachments.thumb_extension AS file_thumb_extension
    FROM tall_posts
    JOIN threads ON tall_posts.thread_id = threads.thread_id
    JOIN boards ON threads.board_id = boards.board_id
    JOIN sites ON sites.site_id = boards.site_id
    LEFT OUTER JOIN attachments ON attachments.post_id = tall_posts.post_id AND attachments.attachment_idx = 1
    ORDER BY bump_time DESC;
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION search_posts(search_text text, max_rows integer DEFAULT 1000)
RETURNS SETOF catalog_grid_result AS $$
    WITH
        query AS (
          SELECT websearch_to_tsquery('english', search_text) AS query
        ),
        result_set AS (
            SELECT
                p.*,
                threads.board_thread_id,
                pathpart,
                sites.name AS site_name,
                sites.site_id AS site_id,
                attachments.mimetype as file_mimetype,
                attachments.illegal as file_illegal,
                attachments.resolution as file_resolution,
                attachments.board_filename as file_name,
                attachments.file_extension,
                attachments.thumb_extension as file_thumb_extension,
                ts_rank(p.body_search_index, query.query) -- TODO: try ts_rank_cd https://www.postgresql.org/docs/current/textsearch-controls.html#TEXTSEARCH-RANKING
                    / (1 + EXTRACT(EPOCH FROM AGE(p.creation_time)) / (3600 * 24)) AS relevance
                FROM posts p
                JOIN threads ON threads.thread_id = p.thread_id
                JOIN boards ON boards.board_id = threads.board_id
                JOIN sites ON sites.site_id = boards.site_id
                LEFT OUTER JOIN attachments
                    ON attachments.post_id = p.post_id
                    AND attachments.attachment_idx = 1
                , query
            WHERE p.body_search_index @@ query.query
            LIMIT max_rows
        )
    SELECT
        0 AS estimated_post_count,
        result_set.post_id,
        result_set.board_post_id,
        result_set.creation_time,
        result_set.creation_time AS bump_time,
        result_set.body,
        result_set.subject,
        result_set.thread_id,
        result_set.embed,
        result_set.board_thread_id,
        result_set.pathpart,
        result_set.site_name,
        result_set.site_id,
        result_set.file_mimetype,
        result_set.file_illegal,
        result_set.file_resolution,
        result_set.file_name,
        result_set.file_extension,
        result_set.file_thumb_extension
    FROM result_set
    ORDER BY result_set.relevance DESC;
$$ LANGUAGE sql STABLE;



SELECT * FROM fetch_catalog(NOW() - INTERVAL '1y', 1001);

SELECT * FROM fetch_catalog(NOW(), 1000);

SELECT count(*) FROM posts;

-- CREATE INDEX idx_posts_thread_board ON posts (thread_id, board_post_id);
ANALYZE posts;
		
		
SELECT DISTINCT ON (top.thread_id)
    top.thread_id,
    posts.post_id,
    posts.body,
    top.post_count,
    top.where_to_leave_off
FROM fetch_top_threads(NOW() - INTERVAL '10d') top
JOIN posts ON top.thread_id = posts.thread_id
ORDER BY top.thread_id, posts.board_post_id;

	
SELECT * FROM threads WHERE thread_id = 3110;
SELECT * FROM posts WHERE thread_id = 3110 ORDER BY board_post_id ASC;
SELECT * FROM boards;
	
SELECT * FROM threads WHERE thread_id = 11314;
ANALYZE posts;

SELECT count(*) from attachments;

SELECT * FROM attachments WHERE post_id = 253383;
SELECT * from attachments WHERE board_filename = '1722466065515';
SELECT count(*) attachments WHERE attachment_id < (SELECT attachment_id FROM attachments WHERE board_filename = '1722466065515');
SELECT max(attachment_id) FROM attachments a;
SELECT pg_get_serial_sequence('attachments', 'attachment_id');
SELECT setval(pg_get_serial_sequence('attachments', 'attachment_id'), COALESCE(198853, 1), true);


UPDATE attachments SET thumb_extension = 'png'
WHERE
	attachment_id IN
	(
		SELECT a.attachment_id
		FROM attachments a
		JOIN posts p ON a.post_id = p.post_id
		JOIN threads t ON p.thread_id = t.thread_id
		JOIN boards b ON t.board_id = b.board_id
		JOIN sites s ON b.site_id = s.site_id
		WHERE s.name = 'leftychan'
		AND a.thumb_extension = 'jpg'
	);


SELECT * FROM posts WHERE board_post_id = 129;
SELECT * FROM attachments WHERE post_id = 461287;

SELECT count(a.*)
FROM attachments a
JOIN posts p ON a.post_id = p.post_id
JOIN threads t ON p.thread_id = t.thread_id
JOIN boards b ON t.board_id = b.board_id
JOIN sites s ON b.site_id = s.site_id
WHERE s.name = 'leftychan'
AND a.thumb_extension = 'jpg';
	

SELECT * FROM posts
JOIN threads ON threads.thread_id = posts.thread_id
JOIN boards ON boards.board_id = threads.board_id
WHERE boards.pathpart = 'leftypol'
	AND boards.site_id = 1
ORDER BY posts.creation_time DESC
LIMIT 1;

SELECT * FROM posts
ORDER BY posts.creation_time DESC 
LIMIT 1;

SELECT boards.board_id, boards.pathpart, sites.name FROM boards JOIN sites ON sites.site_id = boards.site_id;

SELECT DISTINCT ON (b.board_id) 
       b.board_id,
       b.site_id,
       b.pathpart,
       p.post_id,
       p.board_post_id,
       p.creation_time,
       t.thread_id,
       t.board_thread_id
  FROM boards b
  LEFT JOIN threads t ON t.board_id = b.board_id
  LEFT JOIN posts   p ON p.thread_id = t.thread_id AND p.attachment_not_considered = false
  ORDER BY b.board_id, p.creation_time DESC;

-- for Sync
CREATE OR REPLACE FUNCTION get_latest_posts_per_board()
RETURNS TABLE (
    board_id int,
    site_id int,
    pathpart text,
    post_id bigint,
    board_post_id bigint,
    creation_time timestamp with time zone,
    thread_id bigint,
    board_thread_id bigint
) AS $$
    SELECT DISTINCT ON (b.board_id) 
           b.board_id,
           b.site_id,
           b.pathpart,
           p.post_id,
           p.board_post_id,
           p.creation_time,
           t.thread_id,
           t.board_thread_id
      FROM boards b
      LEFT JOIN threads t ON t.board_id = b.board_id
      LEFT JOIN posts   p ON p.thread_id = t.thread_id AND p.attachment_not_considered = false
      ORDER BY b.board_id, p.creation_time DESC;
$$ LANGUAGE sql STABLE;

GRANT EXECUTE ON FUNCTION get_latest_posts_per_board TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION get_latest_posts_per_board    TO chan_archiver;

SELECT * FROM get_latest_posts_per_board();
SELECT * FROM boards JOIN sites ON boards.site_id = sites.site_id WHERE sites.name = 'leftychan';

ALTER TABLE posts ADD COLUMN attachment_not_considered boolean NOT NULL DEFAULT false;

SELECT * FROM posts WHERE board_post_id = 1044;

SELECT DISTINCT ON (p.thread_id) t.*
FROM posts p
JOIN threads t ON t.thread_id = p.thread_id
WHERE t.board_id = 3
ORDER BY p.thread_id DESC, p.creation_time DESC
LIMIT 358;

DROP FUNCTION top_threads_on_board;

CREATE OR REPLACE FUNCTION top_threads_on_board(given_board_id int, max_rows int)
RETURNS SETOF threads AS $$
    SELECT t.*
    FROM threads t
    JOIN (
        SELECT thread_id, MAX(creation_time) AS latest_bump
        FROM posts
        GROUP BY thread_id
    ) p ON t.thread_id = p.thread_id
    WHERE t.board_id = given_board_id
    ORDER BY p.latest_bump DESC
    LIMIT max_rows;
$$ LANGUAGE sql STABLE;

GRANT EXECUTE ON FUNCTION top_threads_on_board                TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION top_threads_on_board          TO chan_archiver;

SELECT * FROM top_threads_on_board(3, 358);

SELECT * FROM posts
	JOIN threads ON posts.thread_id = threads.thread_id
	WHERE board_thread_id = 11701 AND board_id = 3;

SELECT p.* FROM posts p
	JOIN threads ON p.thread_id = threads.thread_id
	WHERE board_id = 3
  AND board_thread_id = 150860
  ORDER BY p.creation_time DESC;

-- this will give us the last post's creation time per thread.
-- (which with merged threads isn't the last post right?)
-- Try to get as close as possible (ideally) perfectly to the order on the board
SELECT t.*, latest_bump
FROM threads t
JOIN (
    SELECT thread_id, MAX(creation_time) AS latest_bump
    FROM posts
    WHERE (sage = false OR local_idx < 2) -- OP can't sage themselves
    AND (local_idx <= 600)
    GROUP BY thread_id
) p ON t.thread_id = p.thread_id
WHERE t.board_id = 3
ORDER BY p.latest_bump DESC
LIMIT 358;

-- Add the 'sage' column with the default value
ALTER TABLE posts ADD COLUMN sage boolean NOT NULL DEFAULT false;

UPDATE posts SET sage = (COALESCE(email, '') = 'sage');

UPDATE posts SET sage = true WHERE LOWER(COALESCE(email, '')) = 'sage';

-- Create an index on the 'sage' column
CREATE INDEX posts_sage_idx ON posts (sage);

DROP FUNCTION IF EXISTS top_threads_on_board;

SELECT t.*, latest_bump
FROM threads t
JOIN (
    SELECT thread_id, MAX(creation_time) AS latest_bump
    FROM posts
    WHERE (sage = false OR local_idx < 2) -- OP can't sage themselves
    AND (local_idx <= 600)
    GROUP BY thread_id
) p ON t.thread_id = p.thread_id
WHERE t.board_id = 3
ORDER BY p.latest_bump DESC
LIMIT 358;

SELECT
    b.board_id,
    b.site_id,
    b.pathpart,
    top_post.post_id,
    top_post.board_post_id,
    top_post.creation_time,
    top_post.thread_id,
    top_post.board_thread_id
FROM boards b
LEFT JOIN LATERAL (
    SELECT
        t.thread_id,
        t.board_thread_id,
        p.post_id,
        p.board_post_id,
        p.creation_time
    FROM threads t
    LEFT JOIN posts p ON p.thread_id = t.thread_id AND p.is_missing_attachments = false
    WHERE t.board_id = b.board_id
    ORDER BY p.creation_time DESC
    LIMIT 1
) AS top_post ON true;


CREATE OR REPLACE FUNCTION get_latest_posts_per_board()
RETURNS TABLE (
    board_id int,
    site_id int,
    pathpart text,
    post_id bigint,
    board_post_id bigint,
    creation_time timestamp with time zone,
    thread_id bigint,
    board_thread_id bigint
) AS $$
    SELECT
        b.board_id,
        b.site_id,
        b.pathpart,
        top_post.post_id,
        top_post.board_post_id,
        top_post.creation_time,
        top_post.thread_id,
        top_post.board_thread_id
    FROM boards b
    LEFT JOIN LATERAL (
        SELECT
            t.thread_id,
            t.board_thread_id,
            p.post_id,
            p.board_post_id,
            p.creation_time
        FROM threads t
        LEFT JOIN posts p ON p.thread_id = t.thread_id AND p.is_missing_attachments = false
        WHERE t.board_id = b.board_id
        ORDER BY p.creation_time DESC
        LIMIT 1
    ) AS top_post ON true;
$$ LANGUAGE sql STABLE;

SELECT * FROM get_latest_posts_per_board();

DROP TRIGGER trigger_update_post_body_search_index ON posts;

ALTER TABLE posts RENAME COLUMN is_missing_attachments TO attachment_not_considered;

SELECT * FROM posts ORDER BY creation_time ASC LIMIT 100;

SELECT DISTINCT ON (b.board_id)
       b.board_id,
       b.site_id,
       b.pathpart,
       p.post_id,
       p.board_post_id,
       p.creation_time,
       t.thread_id,
       t.board_thread_id
  FROM boards b
  LEFT JOIN threads t ON t.board_id = b.board_id
  LEFT JOIN posts   p ON p.thread_id = t.thread_id AND p.attachment_not_considered = false
  ORDER BY b.board_id, p.creation_time DESC;


DELETE FROM threads WHERE thread_id IN
(
	SELECT threads.thread_id FROM threads
	LEFT JOIN posts ON posts.thread_id = threads.thread_id AND posts.attachment_not_considered = false
	WHERE posts.thread_id IS NULL
); .

SELECT * FROM threads
LEFT JOIN posts ON posts.thread_id = threads.thread_id AND posts.attachment_not_considered = false
WHERE posts.thread_id IS NULL;