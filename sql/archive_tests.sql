select count(*) from posts;

select board_id, count(*) as post_count from posts natural join threads group by board_id;

select sum(post_count) from (select board_id, count(*) as post_count from posts join threads on posts.thread_id = threads.thread_id group by threads.board_id) as t;

select board_id, count(*) from threads natural join boards group by board_id;

select count(*) from posts;

SELECT
  threads.thread_id,
  MAX(posts.creation_time) AS bump_time,
  -- FIRST_VALUE(posts.subject) OVER (PARTITION BY threads.thread_id ORDER BY posts.creation_time) AS first_post_subject,
  FIRST_VALUE(posts.body) OVER (PARTITION BY threads.thread_id ORDER BY posts.creation_time) AS first_post_body,
  COUNT(*) AS post_count
FROM
  threads
JOIN
  posts ON threads.thread_id = posts.thread_id
WHERE
  posts.creation_time < NOW()  -- Specify your desired time here
GROUP BY
  threads.thread_id
ORDER BY
  bump_time DESC
LIMIT
  100;
 
 
 SELECT
  thread_data.thread_id,
  thread_data.bump_time,
  thread_data.first_post_subject,
  thread_data.first_post_body,
  COUNT(*) AS post_count
FROM (
    SELECT
      threads.thread_id,
      MAX(posts.creation_time) AS bump_time,
      -- FIRST_VALUE(posts.subject) OVER (PARTITION BY threads.thread_id ORDER BY posts.creation_time) AS first_post_subject,
      FIRST_VALUE(posts.body) OVER (PARTITION BY threads.thread_id ORDER BY posts.creation_time) AS first_post_body,
      posts.creation_time  -- This can be removed if it's not needed in outer query
    FROM
      threads
    JOIN
      posts ON threads.thread_id = posts.thread_id
    WHERE
      posts.creation_time < NOW()  -- Specify your desired time here
) AS thread_data
GROUP BY
  thread_data.thread_id, thread_data.bump_time, thread_data.first_post_subject, thread_data.first_post_body
ORDER BY
  thread_data.bump_time DESC
LIMIT
  100;

 
explain ANALYZE select

-- runs in 1.5s
SELECT
  thread_data.thread_id,
  MAX(thread_data.creation_time) AS bump_time,
  MIN(thread_data.first_post_body) AS first_post_body,  -- Getting the first post's body per thread using MIN as a trick since FIRST_VALUE is the same for all rows per thread in the subquery
  COUNT(*) AS post_count
FROM (
    SELECT
      threads.thread_id,
      posts.creation_time,
      FIRST_VALUE(posts.body) OVER (PARTITION BY threads.thread_id ORDER BY posts.creation_time) AS first_post_body
    FROM
      threads
    JOIN
      posts ON threads.thread_id = posts.thread_id
    WHERE
      posts.creation_time < NOW()  -- Specify your desired time here
) AS thread_data
GROUP BY
  thread_data.thread_id
ORDER BY
  bump_time DESC
LIMIT
  100;


-- runs in 640ms
WITH LatestPosts AS (
    -- Get the latest post time per thread
    SELECT
        p.thread_id,
        MAX(p.creation_time) AS bump_time
    FROM
        posts p
    WHERE
        p.creation_time < NOW() - INTERVAL '365 day'
    GROUP BY
        p.thread_id
    ORDER BY
        bump_time DESC
    LIMIT 100
),
FirstPost AS (
    -- Get the first post's body per thread
    SELECT
        p.thread_id,
        p.body AS first_post_body
    FROM
        posts p
    JOIN
        LatestPosts lp ON p.thread_id = lp.thread_id
    WHERE
        p.creation_time = (
            SELECT MIN(p2.creation_time)
            FROM posts p2
            WHERE p2.thread_id = p.thread_id
        )
)
SELECT
    lp.thread_id,
    lp.bump_time,
    fp.first_post_body,
    COUNT(p.post_id) AS post_count
FROM
    LatestPosts lp
JOIN
    FirstPost fp ON lp.thread_id = fp.thread_id
JOIN
    posts p ON lp.thread_id = p.thread_id
GROUP BY
    lp.thread_id, lp.bump_time, fp.first_post_body
ORDER BY
    lp.bump_time DESC;
   
   
   
   
   
   
   
   
   
   
   
 -- runs in 1.2s
 WITH LatestPosts AS (
    -- Get the latest post time per thread
    SELECT
        p.thread_id,
        MAX(p.creation_time) AS bump_time
    FROM
        posts p
    WHERE
        p.creation_time < NOW() - INTERVAL '365 day'
    GROUP BY
        p.thread_id
    ORDER BY
        bump_time DESC
    LIMIT 100
),
FirstPost AS (
    -- Get the first post's body per thread
    SELECT
        p.thread_id,
        p.body AS first_post_body
    FROM (
        SELECT
            thread_id,
            body,
            RANK() OVER(PARTITION BY thread_id ORDER BY creation_time) as rnk
        FROM
            posts
    ) p
    WHERE
        rnk = 1
    AND
        p.thread_id IN (SELECT thread_id FROM LatestPosts)  -- Optional: limit rows to those from LatestPosts CTE
)
SELECT
    lp.thread_id,
    lp.bump_time,
    fp.first_post_body,
    COUNT(p.post_id) AS post_count
FROM
    LatestPosts lp
JOIN
    FirstPost fp ON lp.thread_id = fp.thread_id
JOIN
    posts p ON lp.thread_id = p.thread_id
GROUP BY
    lp.thread_id, lp.bump_time, fp.first_post_body
ORDER BY
    lp.bump_time DESC;


   
-- runs in ~420ms
WITH LatestPosts AS (
    -- Get the latest post time per thread
    SELECT
        p.thread_id,
        MAX(p.creation_time) AS bump_time
    FROM
        posts p
    WHERE
        p.creation_time < NOW() - interval '365 day'
    GROUP BY
        p.thread_id
    ORDER BY
        bump_time DESC
    LIMIT 100
)
-- Main Query
SELECT
    lp.thread_id,
    lp.bump_time,
    fp.first_post_body,
    COUNT(p.post_id) AS post_count
FROM
    LatestPosts lp
JOIN LATERAL (
    SELECT p.body AS first_post_body
    FROM posts p
    WHERE p.thread_id = lp.thread_id
    ORDER BY p.creation_time
    LIMIT 1
) fp ON TRUE
JOIN
    posts p ON lp.thread_id = p.thread_id
GROUP BY
    lp.thread_id, lp.bump_time, fp.first_post_body
ORDER BY
    lp.bump_time DESC;
   

select sum(counts) from
(
select max(creation_time) as bump_time, count(*) as counts, thread_id
from
	(
		select thread_id, creation_time from posts
		where creation_time < NOW()
		order by creation_time desc
	) as t
group by thread_id
order by bump_time desc
limit 2
) as t;



-- actually we don't need these, because "first" would not be the OP it would be the most recent post. We should just join the results with post
-- to get the OPs

-- Create a function that always returns the first non-NULL value:
CREATE OR REPLACE FUNCTION public.first_agg (anyelement, anyelement)
  RETURNS anyelement
  LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS
'SELECT $1';

-- Then wrap an aggregate around it:
CREATE AGGREGATE public.first (anyelement) (
  SFUNC    = public.first_agg
, STYPE    = anyelement
, PARALLEL = safe
);

------------


(
select max(creation_time) as bump_time, count(*), thread_id, min(creation_time) as where_to_leave_off
from
	(
		select thread_id, creation_time, body from posts
		where creation_time < NOW() - interval '365 day'
		order by creation_time desc limit 1500

	) as t
group by thread_id
order by bump_time desc
)

union all

(
select max(creation_time) as bump_time, count(*), thread_id, min(creation_time) as where_to_leave_off
from
	(
        select thread_id, creation_time, body from posts
		where creation_time < '2022-10-04 12:31:25.000 -0400'
		order by creation_time desc limit 500
	) as t
group by thread_id
order by bump_time desc
);


-- runs in 8ms
select max(creation_time) as bump_time, count(*), thread_id, min(creation_time) as where_to_leave_off
from
	(
		(select thread_id, creation_time, body from posts
		where creation_time < NOW() - interval '365 day'
		order by creation_time desc limit 1500)
		union all
		(select thread_id, creation_time, body from posts
		where creation_time < '2022-10-04 12:31:25.000 -0400'
		order by creation_time desc limit 500)

	) as t
group by thread_id
order by bump_time desc;



   
explain analyze SELECT
        p.thread_id,
        MAX(p.creation_time) AS bump_time
    FROM
        posts p
    WHERE
        p.creation_time < NOW() - interval '365 day'
GROUP BY
    p.thread_id
ORDER BY
    bump_time DESC
LIMIT 100


-- Create temp table if not exists
drop table if exists temp_results;
CREATE TEMP TABLE IF NOT EXISTS temp_results (bump_time TIMESTAMPTZ, post_count INT, thread_id INT, last_fetched_time TIMESTAMP);

/*
 * This function scans backwards from p_start_time until we get the desired number of threads.
 * Since the posts are ordered by creation time, the first post encountered for a particular
 * thread will have that thread's bump time. Any posts after that will have a timestamp
 * further back in time, so thread ids that we encounter will not change order the
 * more we scan.
 *
 * This is to avoid a sequence scan on the entire posts table...
 */
CREATE OR REPLACE FUNCTION fetch_top_threads(p_start_time TIMESTAMPTZ, p_desired_threads INT)
RETURNS TABLE(bump_time TIMESTAMPTZ, post_count INT, thread_id INT)
LANGUAGE plpgsql
AS $$
DECLARE 
    limit_size INT := 2000; -- Start with 2000 posts
    max_iterations INT := 10; -- Maximum iterations to avoid endless loop in case of errors
    result_count INT := 0;
    last_min_time TIMESTAMP;
BEGIN 
    TRUNCATE temp_results; -- clear the table

    FOR i IN 1..max_iterations LOOP 
        -- Fetch posts starting from the previous min time
        INSERT INTO temp_results(bump_time, post_count, thread_id, last_fetched_time)
        SELECT max(t.creation_time) as bump_time, count(*) as post_count, t.thread_id, min(t.creation_time) as last_fetched_time
        FROM (
            SELECT p.thread_id, p.creation_time 
            FROM posts p
            WHERE p.creation_time < COALESCE(last_min_time, p_start_time)
            ORDER BY p.creation_time DESC
            LIMIT limit_size
        ) as t
        GROUP BY t.thread_id;

        -- Check if we have enough threads
        --SELECT temp_results.thread_id INTO result_count FROM temp_results;
       	SELECT COUNT(DISTINCT temp_results.thread_id) INTO result_count FROM temp_results; -- specify the table name here to avoid ambiguity


        IF result_count >= p_desired_threads THEN
            EXIT;
        END IF;

        -- Get the last min time from the temp table for the next iteration
        SELECT MIN(last_fetched_time) INTO last_min_time FROM temp_results;

        -- Double the limit for the next iteration
        limit_size := limit_size * 2;

    END LOOP;

    -- Return the results
    RETURN QUERY SELECT temp_results.bump_time, temp_results.post_count, temp_results.thread_id FROM temp_results ORDER BY temp_results.bump_time DESC;

END;
$$;


select * FROM fetch_top_threads(NOW(), 300) offset 0;

explain analyze WITH TopThreads AS (
    SELECT * FROM fetch_top_threads(NOW() - interval '365d', 100)
)
SELECT 
    tt.thread_id,
    tt.bump_time,
    tt.post_count,
    p.creation_time,
    p.body,
    p.post_id
FROM 
    TopThreads tt
JOIN 
    posts p ON tt.thread_id = p.thread_id
ORDER BY 
    p.creation_time asc;
