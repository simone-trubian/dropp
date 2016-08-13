-- Revert dropp-db:appschema from pg

BEGIN;

DROP SCHEMA v_1;
DROP ROLE IF EXISTS dropp_client;
COMMIT;
