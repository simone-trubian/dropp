-- Revert dropp-db:items from pg

BEGIN;

DROP TABLE v_1.items;

COMMIT;
