-- BEGIN TRANSACTION;

DROP TYPE IF EXISTS catalog_grid_result CASCADE;
DROP FUNCTION IF EXISTS fetch_catalog;

-- OLD: 121ms
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
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION fetch_catalog2(
    selected_time timestamptz,
    board_ids   int[] DEFAULT NULL,
    scroll_time   timestamptz DEFAULT 'infinity'::timestamptz,
    thread_count  int DEFAULT 1000
) RETURNS SETOF catalog_grid_result AS $$
BEGIN
RETURN QUERY WITH active_slices AS (
    SELECT thread_id, board_id, valid_from AS bump_time, post_count
    FROM thread_bump_time_slices
    WHERE valid_from <= selected_time
      AND valid_until > selected_time
      AND valid_from < scroll_time
      AND (board_ids IS NULL OR board_id = ANY(board_ids))
    ORDER BY valid_from DESC
    LIMIT thread_count
)
SELECT 
    s.post_count::bigint AS estimated_post_count,
    p.post_id,
    p.board_post_id,
    p.creation_time,
    s.bump_time,
    p.body,
    p.subject,
    s.thread_id,
    p.embed,
    t.board_thread_id,
    b.pathpart,
    st.name AS site_name,
    st.site_id AS site_id,
    a.mimetype AS file_mimetype,
    a.illegal AS file_illegal,
    a.resolution AS file_resolution,
    a.board_filename AS file_name,
    a.file_extension,
    a.thumb_extension AS file_thumb_extension
FROM active_slices s
JOIN posts p ON p.thread_id = s.thread_id AND p.local_idx = 1
JOIN threads t ON s.thread_id = t.thread_id
JOIN boards b ON t.board_id = b.board_id
JOIN sites st ON b.site_id = st.site_id
LEFT JOIN attachments a ON a.post_id = p.post_id AND a.attachment_idx = 1
ORDER BY s.bump_time DESC;

END
$$ LANGUAGE plpgsql stable;

-- REVOKE EXECUTE ON FUNCTION fetch_catalog FROM PUBLIC;
-- GRANT EXECUTE ON FUNCTION fetch_catalog     TO chan_archive_anon;
-- GRANT EXECUTE ON FUNCTION fetch_catalog                 TO chan_archiver;

-- ROLLBACK;
-- COMMIT;
