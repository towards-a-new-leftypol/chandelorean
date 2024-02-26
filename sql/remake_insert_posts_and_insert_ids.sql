REVOKE EXECUTE ON FUNCTION insert_posts_and_return_ids FROM PUBLIC;
DROP FUNCTION IF EXISTS insert_posts_and_return_ids(new_posts posts[]);

CREATE OR REPLACE FUNCTION insert_posts_and_return_ids(new_posts posts[])
RETURNS TABLE (post_id bigint, board_post_id bigint, thread_id bigint) AS $$
WITH 
selected AS (
    SELECT post_id, board_post_id, thread_id, embed
    FROM posts
    WHERE (thread_id, board_post_id) IN (SELECT thread_id, board_post_id FROM unnest(new_posts))
),
to_update AS (
    SELECT s.post_id, np.embed
    FROM unnest(new_posts) AS np
    JOIN selected s ON np.thread_id = s.thread_id AND np.board_post_id = s.board_post_id
    WHERE s.embed IS DISTINCT FROM np.embed
),
updated AS (
    UPDATE posts p
    SET embed = tu.embed
    FROM to_update tu
    WHERE p.post_id = tu.post_id
    RETURNING p.post_id, p.board_post_id, p.thread_id, p.embed
),
to_insert AS (
    SELECT np.*
    FROM unnest(new_posts) AS np
    LEFT OUTER JOIN selected s ON np.thread_id = s.thread_id AND np.board_post_id = s.board_post_id
    WHERE s.post_id IS NULL
),
inserted AS (
    INSERT INTO posts (board_post_id, creation_time, body, subject, name, email, thread_id, embed)
    SELECT board_post_id, creation_time, body, subject, name, email, thread_id, embed
    FROM to_insert
    RETURNING post_id, board_post_id, thread_id, embed
)
SELECT post_id, board_post_id, thread_id FROM inserted
UNION ALL
SELECT post_id, board_post_id, thread_id FROM updated
UNION ALL
SELECT post_id, board_post_id, thread_id FROM selected WHERE post_id NOT IN (SELECT post_id FROM updated);
$$ LANGUAGE sql;

GRANT EXECUTE ON FUNCTION insert_posts_and_return_ids   TO chan_archiver;
