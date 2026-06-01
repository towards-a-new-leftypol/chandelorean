BEGIN TRANSACTION;

DROP FUNCTION IF EXISTS fetch_catalog2;
DROP TABLE IF EXISTS thread_bump_time_slices CASCADE;

CREATE TABLE IF NOT EXISTS thread_bump_time_slices
    ( slice_id    bigserial primary key
    , thread_id   bigint NOT NULL REFERENCES threads (thread_id) ON DELETE CASCADE
    , board_id    int NOT NULL REFERENCES boards (board_id) ON DELETE CASCADE
    , valid_from  timestamp with time zone NOT NULL
    , valid_until timestamp with time zone NOT NULL DEFAULT 'infinity'::timestamp with time zone
    , post_id     bigint UNIQUE REFERENCES posts (post_id) ON DELETE SET NULL
    , post_count  int NOT NULL DEFAULT 0
    , CONSTRAINT  unique_bump_slice_thread_valid_from UNIQUE (thread_id, valid_from)
    );

-- Board-scoped time-travel pagination (covering index for fast catalog reads)
CREATE INDEX thread_bump_time_slices_board_id_valid_from_idx
    ON thread_bump_time_slices (board_id, valid_from DESC)
    INCLUDE (thread_id, valid_until, post_count);

-- Global/unfiltered time-travel pagination
CREATE INDEX thread_bump_time_slices_valid_from_idx
    ON thread_bump_time_slices (valid_from DESC)
    INCLUDE (thread_id, board_id, valid_until, post_count);

-- Fast lookup for active thread heads (used by insert trigger)
CREATE INDEX thread_bump_time_slices_thread_id_idx
    ON thread_bump_time_slices (thread_id)
    WHERE valid_until = 'infinity'::timestamp with time zone;

-- INSERT: Closes current head, inserts new head with correct board_id & post_count
CREATE OR REPLACE FUNCTION new_bump_time_slice_on_post_trigger()
RETURNS trigger AS $$
BEGIN
    -- 1. Close the currently active slice for this thread
    UPDATE thread_bump_time_slices
    SET valid_until = NEW.creation_time
    WHERE thread_id = NEW.thread_id AND valid_until = 'infinity'::timestamptz;

    -- 2. Insert new slice. MAX(post_count) safely derives the previous count.
    INSERT INTO thread_bump_time_slices
        ( thread_id
        , board_id
        , valid_from
        , valid_until
        , post_id
        , post_count
        )
    SELECT
        NEW.thread_id,
        t.board_id,
        NEW.creation_time,
        'infinity'::timestamptz,
        NEW.post_id,
        COALESCE(
            ( SELECT post_count FROM thread_bump_time_slices
                WHERE thread_id = NEW.thread_id
                    AND valid_from < NEW.creation_time
                ORDER BY valid_from DESC
                LIMIT 1
            ), 0) + 1
    FROM threads t
    WHERE t.thread_id = NEW.thread_id
    ON CONFLICT ON CONSTRAINT unique_bump_slice_thread_valid_from DO NOTHING;

    RETURN NULL;
END
$$ LANGUAGE plpgsql;

-- DELETE: Reactivates predecessor if head, decrements future counts, removes slice by post_id
CREATE OR REPLACE FUNCTION delete_bump_time_slice_on_delete_post_trigger()
RETURNS trigger AS $$
BEGIN
    -- 1. Reactivate predecessor if this was the active head
    -- (Its valid_until was set to OLD.creation_time during insert)
    UPDATE thread_bump_time_slices
    SET valid_until = 'infinity'::timestamptz
    WHERE thread_id = OLD.thread_id AND valid_until = OLD.creation_time;

    -- 2. Decrement post_count for all slices created after the deleted post
    UPDATE thread_bump_time_slices
    SET post_count = post_count - 1
    WHERE thread_id = OLD.thread_id AND valid_from > OLD.creation_time;

    -- 3. Remove the slice explicitly by post_id
    DELETE FROM thread_bump_time_slices WHERE post_id = OLD.post_id;

    RETURN NULL;
END
$$ LANGUAGE plpgsql;

-- Fill the table for the first time
WITH slices AS (
    SELECT
        p.thread_id,
        t.board_id,
        p.creation_time AS valid_from,
        LEAD(p.creation_time) OVER (PARTITION BY p.thread_id ORDER BY p.creation_time, p.post_id) AS valid_until,
        p.post_id,
        COUNT(*) OVER (PARTITION BY p.thread_id ORDER BY p.creation_time, p.post_id) AS post_count
    FROM posts p
    JOIN threads t ON p.thread_id = t.thread_id
)
INSERT INTO thread_bump_time_slices (thread_id, board_id, valid_from, valid_until, post_id, post_count)
SELECT
    thread_id,
    board_id,
    valid_from,
    COALESCE(valid_until, 'infinity'::timestamptz),
    post_id,
    post_count
FROM slices
ON CONFLICT (thread_id, valid_from) DO NOTHING;

CREATE TRIGGER trigger_bump_slice_insert_on_new_post
    AFTER INSERT ON posts
    FOR EACH ROW EXECUTE FUNCTION new_bump_time_slice_on_post_trigger();

CREATE TRIGGER trigger_bump_slice_delete_on_post_delete
    AFTER DELETE ON posts
    FOR EACH ROW EXECUTE FUNCTION delete_bump_time_slice_on_delete_post_trigger();

-- Update planner statistics for optimal query routing
ANALYZE thread_bump_time_slices;

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

EXPLAIN ANALYZE SELECT fetch_catalog2(NOW(), ARRAY[1, 2, 3], thread_count => 50, scroll_time => '2026-05-29 19:57:58-04' :: timestamptz);
EXPLAIN ANALYZE SELECT fetch_catalog2(NOW(), thread_count => 50);
EXPLAIN ANALYZE SELECT fetch_catalog(NOW(), max_row_read => 1000);

EXPLAIN ANALYZE WITH
    selected_time AS (SELECT now()),
    scroll_time AS (SELECT 'infinity' :: timestamptz),
    board_ids AS (SELECT board_id FROM boards),
    active_slices AS
    	(
            SELECT thread_id, board_id, valid_from AS bump_time, post_count
            FROM thread_bump_time_slices
            WHERE valid_from <= (select * from selected_time)
              AND valid_until > (select * from selected_time)
              AND valid_from < (select * from scroll_time)
              AND board_id IN (SELECT * FROM board_ids)
            ORDER BY valid_from DESC
            LIMIT 50
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

GRANT SELECT ON thread_bump_time_slices     TO chan_archive_anon;
GRANT ALL ON thread_bump_time_slices    TO chan_archiver;
REVOKE EXECUTE ON FUNCTION fetch_catalog     FROM chan_archive_anon;
REVOKE EXECUTE ON FUNCTION fetch_catalog2 FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fetch_catalog2    TO chan_archive_anon;
GRANT EXECUTE ON FUNCTION fetch_catalog2                TO chan_archiver;
GRANT usage, select ON SEQUENCE thread_bump_time_slices_slice_id_seq TO chan_archiver;
GRANT EXECUTE ON FUNCTION new_bump_time_slice_on_post_trigger           TO chan_archiver;
GRANT EXECUTE ON FUNCTION delete_bump_time_slice_on_delete_post_trigger TO chan_archiver;

COMMIT;
