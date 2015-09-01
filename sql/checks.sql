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

CREATE OR REPLACE FUNCTION check_user_owns_char (user_id integer, character_id integer) RETURNS boolean
	LANGUAGE plpgsql
	AS $$
	BEGIN
		RETURN EXISTS(
			SELECT 1 FROM "character"
			WHERE "id" = character_id
			AND "user" = user_id
		);
	END;
$$;

CREATE OR REPLACE FUNCTION check_gm_of_char (user_id integer, character_id integer) RETURNS boolean
	LANGUAGE plpgsql
	AS $$
	BEGIN
		RETURN EXISTS(
			SELECT 1
			FROM "character" as c
			INNER JOIN gamemaster as gm
			ON gm."group" = c."group"
			WHERE c."id" = character_id
			AND gm."user" = user_id
		);
	END;
$$;

CREATE OR REPLACE FUNCTION check_wiki_permission_broker (group_id integer, wiki_id integer) RETURNS Integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_broker integer;
	BEGIN
		SELECT permission INTO permission_broker
		FROM wiki as w
		WHERE w.id = wiki_id
		AND w."group" = group_id;

		if permission_broker IS NULL then
			raise exception '[check_wiki_permission_broker] Wiki (%) is not part of specified group (%).', wiki_id, group_id;
		end if;

		return permission_broker;
	END;
$$;

CREATE OR REPLACE FUNCTION check_wiki_permission (character_id integer, wiki_id integer, min_permission integer) RETURNS boolean
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_value integer;
	BEGIN
		SELECT p.permission INTO permission_value
		FROM wiki as w
		INNER JOIN permission as p
		ON p.permission_broker = w.permission
		AND p."character" = character_id
		WHERE w.id = wiki_id;

		RETURN permission_value IS NOT NULL AND permission_value >= min_permission;
	END;
$$;

CREATE OR REPLACE FUNCTION check_history_permission_broker (group_id integer, history_id integer) RETURNS Integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_broker integer;
	BEGIN
		SELECT permission INTO permission_broker
		FROM history as h
		WHERE h.id = history_id
		AND h."group" = group_id;

		if permission_broker IS NULL then
			raise exception '[check_history_permission_broker] History (%) is not part of specified group (%).', history_id, group_id;
		end if;

		return permission_broker;
	END;
$$;

CREATE OR REPLACE FUNCTION check_image_permission_broker (group_id integer, image_id integer) RETURNS Integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_broker integer;
	BEGIN
		SELECT permission INTO permission_broker
		FROM image as i
		WHERE i.id = image_id
		AND i."group" = group_id;

		if permission_broker IS NULL then
			raise exception '[check_image_permission_broker] Image (%) is not part of specified group (%).', image_id, group_id;
		end if;

		return permission_broker;
	END;
$$;

-- One should also have the full permission on a picture, if one has manipulate-permission on the gallery.
CREATE OR REPLACE FUNCTION check_image_permission (character_id integer, image_id integer, min_permission integer) RETURNS boolean
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_value integer;
		min_manipulate_permission integer;
		gallery_id integer;
	BEGIN
		SELECT p.permission INTO permission_value
		FROM image as i
		INNER JOIN permission as p
		ON p.permission_broker = i.permission
		AND p."character" = character_id
		WHERE i.id = image_id;
		
		IF permission_value IS NULL OR permission_value < min_permission THEN
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			SELECT gallery INTO gallery_id
			FROM image as i
			WHERE i.id = image_id;

			RETURN check_gallery_permission(character_id, gallery_id, min_manipulate_permission);
		ELSE
			RETURN TRUE;
		END IF;
		-- RETURN permission_value IS NOT NULL AND permission_value >= min_permission;
	END;
$$;

CREATE OR REPLACE FUNCTION check_gallery_permission (character_id integer, gallery_id integer, min_permission integer) RETURNS boolean
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_value integer;
	BEGIN
		SELECT p.permission INTO permission_value
		FROM gallery as g
		INNER JOIN permission as p
		ON p.permission_broker = g.permission
		AND p."character" = character_id
		WHERE g.id = gallery_id;
		
		RETURN permission_value IS NOT NULL AND permission_value >= min_permission;
	END;
$$;

CREATE OR REPLACE FUNCTION check_gallery_permission_broker (group_id integer, gallery_id integer) RETURNS Integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_broker integer;
	BEGIN
		SELECT permission INTO permission_broker
		from gallery as g
		WHERE g."id" = gallery_id
		AND g."group" = group_id;

		if permission_broker IS NULL then
			raise exception '[check_gallery_permission_broker] Gallery (%) is not part of specified group (%).', gallery_id, group_id;
		end if;

		return permission_broker;
	END;
$$;

