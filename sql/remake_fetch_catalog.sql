BEGIN TRANSACTION;

DROP TYPE IF EXISTS catalog_grid_result CASCADE;
DROP FUNCTION IF EXISTS fetch_catalog;

-- OLD: 121ms
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
        file_mimetype text,
        file_illegal boolean,
        -- file_resolution dimension,
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
        -- sites.site_id,
        attachments.mimetype AS file_mimetype,
        attachments.illegal AS file_illegal,
        -- attachments.resolution AS file_resolution,
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


REVOKE EXECUTE ON FUNCTION fetch_catalog FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fetch_catalog     TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION fetch_catalog                 TO chan_archiver;

-- ROLLBACK;
COMMIT;
