BEGIN TRANSACTION;

DROP TYPE IF EXISTS dimension CASCADE;
DROP TABLE IF EXISTS attachments CASCADE;

CREATE TYPE dimension AS
    ( width  int
    , height int
    );

CREATE TABLE IF NOT EXISTS attachments
    ( attachment_id     bigserial primary key
    , mimetype          text NOT NULL
    , creation_time     timestamp with time zone NOT NULL
    , sha256_hash       text NOT NULL
    , phash             bigint
    , illegal           boolean NOT NULL DEFAULT false
    , post_id           bigint NOT NULL
    , resolution        dimension
    , file_extension    text
    , thumb_extension   text
    , original_filename text
    , board_filename    text NOT NULL
    , spoiler           boolean NOT NULL DEFAULT true
    , file_size_bytes   int
    , CONSTRAINT post_fk FOREIGN KEY (post_id) REFERENCES posts (post_id) ON DELETE CASCADE
    );
CREATE INDEX attachments_creation_time_idx  ON attachments (creation_time);
CREATE INDEX attachments_post_id_idx        ON attachments (post_id);
CREATE INDEX attachments_sha256_hash_idx    ON attachments (sha256_hash);
--
-- Index using the bktree extension for quickly getting the closest phashes
CREATE INDEX attachments_phash_bktree_index ON attachments USING spgist (phash bktree_ops);


GRANT SELECT ON attachments                 TO chan_archive_anon;
GRANT ALL ON attachments                    TO chan_archiver;
GRANT usage, select ON SEQUENCE attachments_attachment_id_seq TO chan_archiver;

COMMIT;
