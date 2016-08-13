-- Verify dropp-db:appschema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('v_1', 'usage');

ROLLBACK;
