WITH indexed_attachments AS (
    SELECT
        attachment_id,
        ROW_NUMBER() OVER (PARTITION BY post_id ORDER BY attachment_id) AS index
    FROM
        attachments
)
UPDATE attachments
SET attachment_idx = indexed_attachments.index
FROM indexed_attachments
WHERE attachments.attachment_id = indexed_attachments.attachment_id;
