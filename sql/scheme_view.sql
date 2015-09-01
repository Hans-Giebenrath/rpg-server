\c rpg;

DROP VIEW IF EXISTS "user";
CREATE VIEW "user" AS
SELECT id, name, email, password
FROM "registered"
WHERE pending = false;

DROP VIEW IF EXISTS "pending";
CREATE VIEW "pending" AS
SELECT id, name, email
FROM "registered"
WHERE pending = true;
