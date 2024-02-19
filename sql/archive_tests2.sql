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
