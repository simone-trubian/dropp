-- Deploy roles
-- requires: appschema
-- requires: views

BEGIN;
DO
$body$
BEGIN
IF NOT EXISTS (
  SELECT *
  FROM   pg_catalog.pg_user
  WHERE  usename = 'dropp_client') THEN

  CREATE ROLE dropp_client LOGIN ;
  ALTER ROLE dropp_client WITH PASSWORD 'pwd';

END IF;
END
$body$;

GRANT SELECT, INSERT, UPDATE, DELETE
ON TABLE v_1.items
TO dropp_client;

GRANT USAGE ON SCHEMA v_1 to dropp_client;
GRANT USAGE ON SEQUENCE v_1.items_id_seq TO dropp_client;

COMMIT;
