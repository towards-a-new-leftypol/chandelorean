BEGIN TRANSACTION;

DROP FUNCTION IF EXISTS search_posts;

-- CREATE TYPE catalog_grid_result AS
--     (
--         -- post_count bigint,
--         estimated_post_count bigint,
--         post_id bigint,
--         board_post_id bigint,
--         creation_time timestamptz,
--         bump_time timestamptz,
--         body text,
--         subject text,
--         thread_id bigint,
--         embed text,
--         board_thread_id bigint,
--         pathpart text,
--         site_name text,
--         file_mimetype text,
--         file_illegal boolean,
--         -- file_resolution dimension,
--         file_name text,
--         file_extension text,
--         file_thumb_extension text
--     );

CREATE OR REPLACE FUNCTION search_posts(search_text text)
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
                attachments.mimetype as file_mimetype,
                attachments.illegal as file_illegal,
                -- attachments.resolution as file_resolution,
                attachments.board_filename as file_name,
                attachments.file_extension,
                attachments.thumb_extension as file_thumb_extension,
                ts_rank(p.body_search_index, query.query)
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
            LIMIT 2000
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
        result_set.file_mimetype,
        result_set.file_illegal,
        result_set.file_name,
        result_set.file_extension,
        result_set.file_thumb_extension
    FROM result_set
    ORDER BY result_set.relevance DESC;
$$ LANGUAGE sql STABLE;

GRANT EXECUTE ON FUNCTION search_posts     TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION search_posts                 TO chan_archiver;
REVOKE EXECUTE ON FUNCTION search_posts FROM PUBLIC;

COMMIT;
