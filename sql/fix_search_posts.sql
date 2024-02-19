BEGIN TRANSACTION;

DROP FUNCTION IF EXISTS search_posts;

CREATE OR REPLACE FUNCTION search_posts(search_text text)
RETURNS TABLE (
  post posts,
  pathpart text,
  board_thread_id bigint,
  relevance double precision
) AS $$
WITH query AS (
  SELECT websearch_to_tsquery('english', search_text) AS query
)
SELECT p::posts AS post, pathpart, board_thread_id,
	ts_rank(p.body_search_index, query.query)
	/ (1 + EXTRACT(EPOCH FROM AGE(p.creation_time)) / (3600 * 24))	AS relevance
FROM posts p JOIN threads ON threads.thread_id = p.thread_id JOIN boards ON boards.board_id = threads.board_id, query
WHERE p.body_search_index @@ query.query
ORDER BY relevance
DESC
$$ LANGUAGE sql STABLE;

GRANT EXECUTE ON FUNCTION search_posts     TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION search_posts                 TO chan_archiver;
REVOKE EXECUTE ON FUNCTION search_posts FROM PUBLIC;

ROLLBACK;
