WITH indexed_posts AS (
    SELECT
        post_id,
        ROW_NUMBER() OVER (PARTITION BY thread_id ORDER BY board_post_id) AS index
    FROM
        posts
)
UPDATE posts
SET local_idx = indexed_posts.index
FROM indexed_posts
WHERE posts.post_id = indexed_posts.post_id;
