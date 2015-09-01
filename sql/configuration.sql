\c rpg;

DROP TABLE IF EXISTS "config" CASCADE;
CREATE TABLE IF NOT EXISTS "config" (
	id integer not null DEFAULT 1,
	min_view_permission integer not null,
	min_manipulate_permission integer not null,
	min_delete_permission integer not null,
	creator_permission integer not null
);

INSERT INTO "config" (min_view_permission, min_manipulate_permission, min_delete_permission, creator_permission) VALUES (100, 200, 300, 500);

CREATE OR REPLACE FUNCTION config_min_view_permission () RETURNS integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		p integer;
	BEGIN
		SELECT min_view_permission INTO p
		FROM "config"
		WHERE id = 1;

		RETURN p;
	END;
$$;

CREATE OR REPLACE FUNCTION config_min_manipulate_permission () RETURNS integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		p integer;
	BEGIN
		SELECT min_manipulate_permission INTO p
		FROM "config"
		WHERE id = 1;

		RETURN p;
	END;
$$;

CREATE OR REPLACE FUNCTION config_min_delete_permission () RETURNS integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		p integer;
	BEGIN
		SELECT min_delete_permission INTO p
		FROM "config"
		WHERE id = 1;

		RETURN p;
	END;
$$;

CREATE OR REPLACE FUNCTION config_creator_permission () RETURNS integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		p integer;
	BEGIN
		SELECT creator_permission INTO p
		FROM "config"
		WHERE id = 1;

		RETURN p;
	END;
$$;
