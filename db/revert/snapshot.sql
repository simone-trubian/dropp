-- Revert dropp-db:snapshot from pg

BEGIN;

DROP TYPE IF EXISTS ebay_status;

DROP TYPE IF EXISTS availability;

COMMIT;
