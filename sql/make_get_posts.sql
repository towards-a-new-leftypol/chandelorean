BEGIN TRANSACTION;

DROP TYPE IF EXISTS post_key CASCADE;

-- DROP FUNCTION IF EXISTS get_posts;

CREATE TYPE post_key AS
    ( thread_id bigint
    , board_post_id bigint
    );

CREATE OR REPLACE FUNCTION get_posts(board_posts post_key[])
RETURNS SETOF posts AS $$

    SELECT *
    FROM posts
    WHERE (thread_id, board_post_id) IN (SELECT thread_id, board_post_id FROM unnest(board_posts))

$$ LANGUAGE sql;

GRANT EXECUTE ON FUNCTION get_posts     TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION get_posts                 TO chan_archiver;
REVOKE EXECUTE ON FUNCTION get_posts FROM PUBLIC;

COMMIT;
