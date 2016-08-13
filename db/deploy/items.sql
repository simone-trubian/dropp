-- Deploy dropp-db:items to pg
-- requires: appschema

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE v_1.items (
    id SERIAL,
    source_url text PRIMARY KEY,
    ebay_url text,
    item_name text
);

COMMIT;
