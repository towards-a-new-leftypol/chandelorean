BEGIN TRANSACTION;

CREATE TABLE IF NOT EXISTS boards
    ( board_id serial primary key
    , name text NOT NULL
    , pathpart text NOT NULL -- if it's /a/ then the pathpart is a
    );

CREATE TABLE IF NOT EXISTS thread
    ( thread_id bigserial primary key
    , creation_time timestamp with time zone NOT NULL
    , board_id int NOT NULL
    , CONSTRAINT board_fk FOREIGN KEY (board_id) REFERENCES boards (board_id) ON DELETE CASCADE
    );

CREATE TABLE IF NOT EXISTS posts
    ( post_id bigserial primary key
    , creation_time timestamp with time zone NOT NULL
    , body text
    , body_search_index tsvector
    , thread_id bigint NOT NULL
    , CONSTRAINT thread_fk FOREIGN KEY (thread_id) REFERENCES thread (thread_id) ON DELETE CASCADE
    );
CREATE INDEX post_body_search_idx ON posts USING GIN (body_search_index);

CREATE OR REPLACE FUNCTION update_post_body_search_index() RETURNS trigger AS $$
BEGIN
    NEW.body_search_index := to_tsvector('english', NEW.body);
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_update_post_body_search_index
BEFORE INSERT OR UPDATE
ON posts
FOR EACH ROW
EXECUTE FUNCTION update_post_body_search_index();

ROLLBACK;
