-- Deploy dropp-db:snapshot to pg
-- requires: items
-- requires: roles

BEGIN;

CREATE TYPE ebay_status AS ENUM ('on', 'off');

CREATE TYPE availability AS ENUM ('available', 'low', 'out');

COMMIT;
