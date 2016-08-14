-- Revert dropp-db:snapshot from pg

BEGIN;

DROP TABLE v_1.snapshot;

DROP TYPE IF EXISTS ebay_status;

DROP TYPE IF EXISTS availability;

COMMIT;
