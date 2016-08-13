-- Verify roles

BEGIN;

SELECT 1 FROM pg_roles WHERE rolname='dropp_client';

ROLLBACK;
