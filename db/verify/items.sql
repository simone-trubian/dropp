-- Verify dropp-db:items on pg

BEGIN;

SELECT id, source_url, ebay_url, item_name
    FROM v_1.items
    WHERE FALSE;

ROLLBACK;
