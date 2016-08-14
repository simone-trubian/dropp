-- Verify dropp-db:snapshot on pg

BEGIN;

SELECT 1 FROM pg_type WHERE typname = 'ebay_status';

SELECT 1 FROM pg_type WHERE typname = 'availability';


SELECT id, ebay_status, availability
FROM v_1.snapshot
WHERE FALSE;

ROLLBACK;
