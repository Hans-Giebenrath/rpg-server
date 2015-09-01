\c rpg;
-- load_characters and load_gms should directly be called after login,
-- so the user can switch its role.
CREATE OR REPLACE FUNCTION own_characters_list (user_id integer, group_id_p integer, character_id_p integer) RETURNS table (character_id integer, character_name text, character_short text, group_id integer, group_name text)
	LANGUAGE plpgsql
	AS $$
	BEGIN
		-- todo does not return correct result ...
		RETURN QUERY SELECT c.id, c.name, c.short, g.id, g.name
		FROM "character" as c
		INNER JOIN "group" as g
		ON g.id=c."group"
		WHERE c."user"=user_id;
	END;
$$;

-- load_characters and load_gms should directly be called after login,
-- so the user can switch its role.
CREATE OR REPLACE FUNCTION own_gms_list (user_id integer, group_id_p integer, character_id_p integer) RETURNS table (group_id integer, group_name text, group_gallery_broker integer)
	LANGUAGE plpgsql
	AS $$
	BEGIN
		RETURN QUERY SELECT g.id, g.name, g.gallery
		FROM "group" as g
		INNER JOIN gamemaster as gm
		ON gm."group" = g.id
		AND gm."user" = user_id;
	end;
$$;
		
CREATE OR REPLACE FUNCTION group_characters_list (user_id_p integer, group_id integer, character_id integer) RETURNS table (character_id_out integer, character_name text, character_short text, character_content text, character_image text, character_gallery integer, user_id integer, user_name text)
	LANGUAGE plpgsql
	AS $$
	BEGIN
		-- todo does not return correct result ...
		RETURN QUERY SELECT c.id, c.name, strip_priv(c.short, c.id, character_id), strip_priv(c.content, c.id, character_id), c.image, c.gallery, u.id, u.name
		FROM "character" as c
		INNER JOIN "user" as u
		ON u.id=c."user"
		WHERE c."group"=group_id;
	END;
$$;
		
CREATE OR REPLACE FUNCTION group_gms_list (user_id_p integer, group_id integer, character_id_p integer) RETURNS table (user_id integer, user_name text)
	LANGUAGE plpgsql
	AS $$
	BEGIN
		-- todo does not return correct result ...
		RETURN QUERY SELECT g."user", u.name
		FROM "gamemaster" as g
		INNER JOIN "user" as u
		ON u.id=g."user"
		WHERE g."group"=group_id;
	END;
$$;

CREATE OR REPLACE FUNCTION gallery_list (user_id_p integer, group_id integer, character_id integer) RETURNS table (id integer, title text, content text, gallery_broker integer, permission integer, permission_broker integer, image_url text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
		permission_value integer;
		min_manipulate_permission integer;
	BEGIN
		-- do not check for equality of visitor and owner:
		-- maybe the gamemaster want's to have an own gallery stored in the character's
		-- profile.
		if character_id is not null then
			SELECT config_min_view_permission () INTO min_view_permission;
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			-- select just galleries visible to the user
			RETURN QUERY SELECT g.id, g.title, strip_priv_pcompare(g.content, p.permission, min_manipulate_permission), g.gallery, p.permission, g.permission, g.image
			FROM "gallery" as g
			INNER JOIN permission as p
			ON p.permission_broker = g.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE g."group" = group_id; -- surely redundant, but i hope for a huge performance gain ...
		else
			RETURN QUERY SELECT g.id, g.title, strip_priv_pcompare(g.content, 0, 0), g.gallery, 0, g.permission, g.image
			FROM "gallery" as g
			WHERE g."group" = group_id;
		end if;
	end;
$$;

-- NOTE the permissions here are somewhat strange. If a user has manipulate-permission on the gallery, then he should have manipulate permission on every picture.
CREATE OR REPLACE FUNCTION image_list (user_id_p integer, group_id integer, character_id integer, gallery_id integer) RETURNS table (id integer, "order" integer, title text, content text, url text, orig text, permission integer, permission_broker integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
		permission_value integer;
		min_manipulate_permission integer;
		strip_content boolean;
	BEGIN
		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			
			-- SELECT NOT check_gallery_permission(character_id, gallery_id, min_manipulate_permission) INTO strip_content;
			strip_content := NOT check_gallery_permission(character_id, gallery_id, min_manipulate_permission);
		ELSE
			strip_content := FALSE;
		END IF;

		IF strip_content THEN
			SELECT config_min_view_permission () INTO min_view_permission;
			-- select just galleries visible to the user
			RETURN QUERY SELECT i.id, i."order", i.title, strip_priv_pcompare(i.content, p.permission, min_manipulate_permission), i.url, i.original, p.permission, i.permission
			FROM "image" as i
			INNER JOIN permission as p
			ON p.permission_broker = i.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			-- The permissions can just be granted, if user is in group
			-- INNER JOIN gallery as g
			-- ON g.gallery = i.gallery
			-- AND g."group" = group_id
			WHERE i.gallery = gallery_id
			AND i."group" = group_id;
		else
			RETURN QUERY SELECT i.id, i."order", i.title, strip_priv_pcompare(i.content, 0, 0), i.url, i.original, 0, i.permission
			FROM "image" as i
			WHERE i.gallery = gallery_id
			AND i."group" = group_id;
		end if;
	end;
$$;

CREATE OR REPLACE FUNCTION image_order_list (user_id_p integer, group_id integer, character_id integer, gallery_id integer) RETURNS table (image_id integer, image_order integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
		min_manipulate_permission integer;
		strip_content boolean;
	BEGIN
		-- do not check for equality of visitor and owner:
		-- maybe the gamemaster want's to have an own gallery stored in the character's
		-- profile.
		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			
			-- SELECT NOT check_gallery_permission(character_id, gallery_id, min_manipulate_permission) INTO strip_content;
			strip_content := NOT check_gallery_permission(character_id, gallery_id, min_manipulate_permission);
		ELSE
			strip_content := FALSE;
		END IF;

		if strip_content then
			SELECT config_min_view_permission () INTO min_view_permission;
			-- select just galleries visible to the user
			RETURN QUERY SELECT -- NOTE let case - otherwise he may would retrieve columns to check in function?
				i.id, i."order"
			FROM image as i
			INNER JOIN permission as p
			ON p.permission_broker = i.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE i.gallery = gallery_id;
		else
			RETURN QUERY SELECT
				i.id, i."order"
			FROM image as i
			WHERE i."gallery" = gallery_id;
		end if;
	END;
$$;

-- content too big for list ...
-- group is stored in client application. Do not check for this.
-- return permission for displaying of controls (so viewing does not show edit-symbols)
CREATE OR REPLACE FUNCTION wiki_list (user_id_p integer, group_id integer, character_id integer) RETURNS table (id integer, title text, slug text, short text, created timestamp, permission integer, permission_broker integer, tags text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		min_view_permission integer;
		permission_value integer;
	BEGIN
		if character_id is not null then
			SELECT config_min_view_permission () INTO min_view_permission;
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			RETURN QUERY SELECT w.id, w.title, w.slug, strip_priv_pcompare(w.short, p.permission, min_manipulate_permission), w.created, p.permission, w.permission, (SELECT string_agg(nm.tag::text, ',') FROM tag_nm as nm WHERE nm.wiki=w.id)
			FROM wiki as w
			INNER JOIN permission as p
			ON p.permission_broker = w.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE w."group"=group_id;
		else
			RETURN QUERY SELECT w.id, w.title, w.slug, strip_priv_pcompare(w.short, 0,0), w.created, 0, w.permission, (SELECT string_agg(nm.tag::text, ',') FROM tag_nm as nm WHERE nm.wiki=w.id)
			FROM wiki as w
			WHERE w."group" = group_id;
		end if;
	END;
$$;

CREATE OR REPLACE FUNCTION tag_list (user_id_p integer, group_id integer, character_id integer) RETURNS table (id integer, name text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
	BEGIN
		IF character_id IS NOT NULL THEN
			SELECT config_min_view_permission () INTO min_view_permission;
			RETURN QUERY SELECT DISTINCT t.id, t.tag
			FROM tag AS t
			INNER JOIN tag_nm AS nm
			ON nm.tag = t.id
			INNER JOIN wiki AS w
			ON w.id = nm.wiki
			INNER JOIN permission as p
			ON p.permission_broker = w.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE w."group"=group_id;
		ELSE
			RETURN QUERY SELECT DISTINCT t.id, t.tag
			FROM tag AS t
			INNER JOIN tag_nm AS nm
			ON nm.tag = t.id
			INNER JOIN wiki AS w
			ON w.id =nm.wiki
			WHERE w."group" = group_id;
		END IF;
	END;
$$;

CREATE OR REPLACE FUNCTION history_list (user_id_p integer, group_id integer, character_id integer) RETURNS table (id integer, title text, short text, created timestamp, "date_ingame" text, "date_outgame" text, "order" integer, permission integer, permission_broker integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		min_view_permission integer;
		permission_value integer;
	BEGIN
		if character_id is not null then
			SELECT config_min_view_permission () INTO min_view_permission;
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			RETURN QUERY SELECT
				h.id,
				h.title,
				strip_priv_pcompare(h.short, p.permission, min_manipulate_permission),
				h.created,
				h."date_ingame",
				h."date_outgame",
				h."order",
				p.permission,
				h.permission
			FROM history as h
			INNER JOIN permission as p
			ON p.permission_broker = h.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE h."group"=group_id;
		else
			RETURN QUERY SELECT
				h.id,
				h.title,
				strip_priv_pcompare(h.short, 0,0),
				h.created,
				h."date_ingame",
				h."date_outgame",
				h."order",
				0,
				h.permission
			FROM history as h
			WHERE h."group" = group_id;
		end if;
	END;
$$;

CREATE OR REPLACE FUNCTION history_order_list (user_id_p integer, group_id integer, character_id integer) RETURNS table (history_id integer, history_order integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
	BEGIN
		-- do not check for equality of visitor and owner:
		-- maybe the gamemaster want's to have an own gallery stored in the character's
		-- profile.
		if character_id is not null then
			SELECT config_min_view_permission () INTO min_view_permission;
			-- select just galleries visible to the user
			RETURN QUERY SELECT -- NOTE let case - otherwise he may would retrieve columns to check in function?
				h.id, h."order"
			FROM history as h
			INNER JOIN permission as p
			ON p.permission_broker = h.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE h."group" = group_id;
		else
			RETURN QUERY SELECT
				h.id, h."order"
			FROM "history" as h
			WHERE h."group" = group_id;
		end if;
	END;
$$;

CREATE OR REPLACE FUNCTION user_list (user_id_p integer, group_id_p integer, character_id_p integer) RETURNS table (id integer, name text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
	BEGIN
		RETURN QUERY SELECT u.id, u.name
		FROM "registered" as u
		ORDER BY u.name;
	END;
$$;

CREATE OR REPLACE FUNCTION permission_list (user_id_p integer, group_id integer, character_id integer, permission_broker_id integer) RETURNS table (character_id_out integer, permission_broker integer, permission integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer; -- well, readers should not be interested in other's permissions
	BEGIN
		SELECT config_min_manipulate_permission() INTO min_manipulate_permission;

		IF character_id IS NOT NULL THEN
			IF NOT EXISTS (
				SELECT 1 from "permission" as p
				WHERE p.permission_broker = permission_broker_id
				AND p.permission >= min_manipulate_permission
				AND p."character" = character_id)	then
				return;
				-- RAISE EXCEPTION '[permission_list] You (char_id: %) are not allowed to list the permissions (%).', character_id, permission_broker_id;
			END IF;
		END IF;

		RETURN QUERY SELECT p."character", permission_broker_id, p.permission
		FROM permission AS p
		WHERE p.permission_broker = permission_broker_id;
	END;
$$;

CREATE OR REPLACE FUNCTION appointment_list (user_id_p integer, group_id integer, character_id_p integer) RETURNS table (id integer, "date" text, elected boolean, electorates text)
	LANGUAGE plpgsql
	AS $$
	BEGIN
		RETURN QUERY SELECT a.id, a."date", a.elected, (SELECT string_agg(ae."user"::text, ',') FROM appointment_electorate AS ae WHERE ae.appointment = a.id)
		FROM appointment AS a
		WHERE a."group" = group_id;
	END;
$$;
