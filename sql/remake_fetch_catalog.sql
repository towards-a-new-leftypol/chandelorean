BEGIN TRANSACTION;

DROP FUNCTION IF EXISTS fetch_catalog;

CREATE OR REPLACE FUNCTION fetch_catalog(max_time timestamptz, max_row_read int DEFAULT 10000)
RETURNS TABLE (
    -- post_count bigint,
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
    site_name text,
    file_mimetype text,
    file_illegal boolean,
    -- file_resolution dimension,
    file_name text,
    file_extension text,
    file_thumb_extension text
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
        )-- ,
        -- post_counts AS
        -- (
        --     SELECT thread_id, count(*) as post_count FROM
        --     tall_posts
        --     GROUP BY thread_id
        -- )
    SELECT
        -- post_counts.post_count,
        op_posts.*,
        threads.board_thread_id, -- this should be part of the url path when creating links, not thread_id (that's internal)
        boards.pathpart,
        sites."name",
        -- sites.site_id,
        attachments.mimetype as file_mimetype,
        attachments.illegal as file_illegal,
        -- attachments.resolution as file_resolution,
        attachments.board_filename as file_name,
        attachments.file_extension,
        attachments.thumb_extension as file_thumb_extension
    FROM op_posts
    -- JOIN post_counts ON op_posts.thread_id = post_counts.thread_id
    JOIN threads ON op_posts.thread_id = threads.thread_id
    JOIN boards ON threads.board_id = boards.board_id
    JOIN sites ON sites.site_id = boards.site_id
    LEFT OUTER JOIN attachments ON attachments.post_id = op_posts.post_id
    ORDER BY bump_time DESC;
$$ LANGUAGE sql;


REVOKE EXECUTE ON FUNCTION fetch_catalog FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fetch_catalog     TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION fetch_catalog                 TO chan_archiver;

-- ROLLBACK;
COMMIT;
