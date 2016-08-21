-- Deploy dropp-db:snapshot to pg
-- requires: items
-- requires: roles

BEGIN;

CREATE TYPE ebay_status AS ENUM ('on', 'off');

CREATE TYPE availability AS ENUM ('available', 'low', 'out');

CREATE TABLE v_1.snapshot (
    id int PRIMARY KEY,
    ebay_status ebay_status,
    availability availability
);

GRANT SELECT, INSERT, UPDATE, DELETE
ON TABLE v_1.snapshot
TO dropp_client;

COMMIT;
