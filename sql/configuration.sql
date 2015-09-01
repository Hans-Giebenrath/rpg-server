-- Copyright (C) 2015 Hans Giebenrath
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>

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
