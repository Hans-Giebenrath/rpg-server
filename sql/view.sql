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

-- NOTE print everything, which is possible to edit
-- NOTE as the stuff also will be called on creation, also return every stuff related to creation

CREATE OR REPLACE FUNCTION user_view (user_id_p integer, group_id_p integer, character_id_p integer, changed_user integer, OUT id_out integer, OUT name_out text) RETURNS record
	LANGUAGE plpgsql
	AS $$
	BEGIN
		SELECT changed_user, u."name" INTO id_out, name_out
		FROM "registered" as u
		WHERE u."id"=changed_user;
	END;
$$;
		
-- this method is called at startup to know the groups' name and gallerybroker. The name should not change.
CREATE OR REPLACE FUNCTION group_view (user_id_p integer, group_id integer, character_id_p integer, OUT name_out text, OUT gallery_broker_out integer) RETURNS record
	LANGUAGE plpgsql
	AS $$
	BEGIN
		SELECT g."name", g.gallery INTO name_out, gallery_broker_out
		FROM "group" as g
		WHERE g."id"=group_id;
	END;
$$;
		
CREATE OR REPLACE FUNCTION wiki_view (user_id_p integer, group_id integer, character_id integer, wiki_id integer) RETURNS table (id_out integer, permission_out integer, permission_broker_out integer, created_out timestamp, slug_out text, title text, short_out text, short_raw_out text, content_out text, content_raw_out text, tags text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
		min_manipulate_permission integer;
	BEGIN
		-- do not check for equality of visitor and owner:
		-- maybe the gamemaster want's to have an own gallery stored in the character's
		-- profile.
		if character_id is not null then
			SELECT config_min_view_permission () INTO min_view_permission;
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			-- select just galleries visible to the user
			RETURN QUERY SELECT -- NOTE let case - otherwise he may would retrieve columns to check in function?
				wiki_id,
				p.permission,
				w.permission,
				w.created,
				w.slug,
				w.title,
				strip_priv_pcompare(w.short, p.permission, min_manipulate_permission),
				CASE WHEN p.permission >= min_manipulate_permission THEN w.short_raw ELSE '' END,
				strip_priv_pcompare(w.content, p.permission, min_manipulate_permission),
				CASE WHEN p.permission >= min_manipulate_permission THEN w.content_raw ELSE '' END,
				(SELECT string_agg(nm.tag::text, ',') FROM tag_nm as nm WHERE nm.wiki=w.id)
			-- INTO id_out, permission_out, permission_broker_out, created_out, slug_out, title, short_out, short_raw_out, content_out, content_raw_out, tags
			FROM wiki as w
			INNER JOIN permission as p
			ON p.permission_broker = w.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE w.id=wiki_id
			LIMIT 1;
		else
			RETURN QUERY SELECT
				wiki_id,
				0,
				w.permission,
				w.created,
				w.slug,
				w.title,
				strip_priv_pcompare(w.short,0,0),
				w.short_raw,
				strip_priv_pcompare(w.content,0,0),
				w.content_raw,
				(SELECT string_agg(nm.tag::text, ',') FROM tag_nm as nm WHERE nm.wiki=w.id)
			-- INTO id_out, permission_out, permission_broker_out, created_out, slug_out, title, short_out, short_raw_out, content_out, content_raw_out, tags
			FROM "wiki" as w
			WHERE w.id = wiki_id
			LIMIT 1;
		end if;
	END;
$$;

CREATE OR REPLACE FUNCTION history_view (user_id_p integer, group_id integer, character_id integer, history_id integer) RETURNS table(id_out integer, permission_out integer, permission_broker_out integer, created_out timestamp, date_ingame_out text, date_outgame_out text, order_out integer, title text, short_out text, short_raw_out text, content_out text, content_raw_out text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
		min_manipulate_permission integer;
	BEGIN
		-- do not check for equality of visitor and owner:
		-- maybe the gamemaster want's to have an own gallery stored in the character's
		-- profile.
		if character_id is not null then
			SELECT config_min_view_permission () INTO min_view_permission;
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			-- select just galleries visible to the user
			RETURN QUERY SELECT -- NOTE let case - otherwise he may would retrieve columns to check in function?
				h.id,
				p.permission,
				h.permission,
				h.created,
				h."date_ingame",
				h."date_outgame",
				h."order",
				h.title,
				strip_priv_pcompare(h.short, p.permission, min_manipulate_permission),
				CASE WHEN p.permission >= min_manipulate_permission THEN h.short_raw ELSE '' END,
				strip_priv_pcompare(h.content, p.permission, min_manipulate_permission),
				CASE WHEN p.permission >= min_manipulate_permission THEN h.content_raw ELSE '' END
			-- INTO id_out, permission_out, permission_broker_out, created_out, date_out, order_out, title, short_out, short_raw_out, content_out, content_raw_out
			FROM history as h
			INNER JOIN permission as p
			ON p.permission_broker = h.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE h.id=history_id
			LIMIT 1;
		else
			RETURN QUERY SELECT
				h.id,
				0,
				h.permission,
				h.created,
				h."date_ingame",
				h."date_outgame",
				h."order",
				h.title,
				strip_priv_pcompare(h.short,0,0),
				h.short_raw,
				strip_priv_pcompare(h.content,0,0),
				h.content_raw
			-- INTO id_out, permission_out, permission_broker_out, created_out, date_out, order_out, title, short_out, short_raw_out, content_out, content_raw_out
			FROM "history" as h
			WHERE h.id = history_id
			LIMIT 1;
		end if;
	END;
$$;

-- as this function will be called on a manipulation of any group character, one has to supply owner
CREATE OR REPLACE FUNCTION character_view (user_id_p integer, group_id integer, character_id integer, owner_id integer, OUT id_out integer, OUT user_out integer, OUT group_out integer, OUT gallery_out integer, OUT name_out text, OUT image_out text, OUT short_out text, OUT short_raw_out text, OUT content_out text, OUT content_raw_out text) RETURNS record
	LANGUAGE plpgsql
	AS $$
	DECLARE
	BEGIN
		-- do not check for equality of visitor and owner:
		-- maybe the gamemaster want's to have an own gallery stored in the character's
		-- profile.
		if character_id is not null then
			-- select just galleries visible to the user
			SELECT -- NOTE let case - otherwise he may would retrieve columns to check in function?
				id,
				"user",
				"group",
				"gallery",
				name,
				image,
				strip_priv(short, character_id, owner_id),
				CASE WHEN character_id = owner_id THEN short_raw ELSE '' END,
				strip_priv(content, character_id, id),
				CASE WHEN character_id = owner_id THEN content_raw ELSE '' END
			INTO id_out, user_out, group_out, gallery_out, name_out, image_out, short_out, short_raw_out, content_out, content_raw_out
			FROM "character" as c
			WHERE c.id = owner_id
			AND c."group" = group_id;
		ELSE
			SELECT
				id,
				"user",
				"group",
				"gallery",
				name,
				image,
				short,
				'',
				content,
				''
			INTO id_out, user_out, group_out, gallery_out, name_out, image_out, short_out, short_raw_out, content_out, content_raw_out
			FROM "character" as c
			WHERE c.id = owner_id
			AND c."group" = group_id;
		END IF;
	END;
$$;

-- TODO also create some stuff for gallery and image
CREATE OR REPLACE FUNCTION gallery_view (user_id_p integer, group_id integer, character_id integer, gallery_id integer) RETURNS table (id_out integer, permission_out integer, permission_broker_out integer, gallery_broker_out integer, title_out text, content_out text, content_raw_out text, image_url_out text)
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
			RETURN QUERY SELECT 
				g.id,
				p.permission,
				g.permission,
				g.gallery,
				g.title,
				strip_priv_pcompare(g.content, p.permission, min_manipulate_permission),
				CASE WHEN p.permission >= min_manipulate_permission THEN g.content_raw ELSE '' END,
				g.image
			-- INTO id_out, permission_out, permission_broker_out, gallery_broker_out, title_out, content_out, content_raw_out, image_url_out
			FROM "gallery" as g
			INNER JOIN permission as p
			ON p.permission_broker = g.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE g.id = gallery_id
			LIMIT 1;
		else
			RETURN QUERY SELECT 
				g.id,
				0,
				g.permission,
				g.gallery,
				g.title,
				strip_priv_pcompare(g.content, 0, 0),
				g.content_raw,
				g.image
			-- INTO id_out, permission_out, permission_broker_out, gallery_broker_out, title_out, content_out, content_raw_out, image_url_out
			FROM "gallery" as g
			WHERE g.id = gallery_id
			LIMIT 1;
		end if;
	end;
$$;

-- NOTE url and original are used for newly created images, which are pushed to the client. So don't optimize away, motherfucker!
CREATE OR REPLACE FUNCTION image_view (user_id_p integer, group_id integer, character_id integer, image_id integer) RETURNS table (id_out integer, gallery_out integer, permission_out integer, permission_broker_out integer, url_out text, original_out text, title_out text, content_out text, content_raw_out text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_view_permission integer;
		permission_value integer;
		min_manipulate_permission integer;
	BEGIN
		if character_id is not null then
			SELECT config_min_view_permission () INTO min_view_permission;
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			-- select just galleries visible to the user
			RETURN QUERY SELECT
				i.id,
				i.gallery,
				p.permission,
				i.permission,
				i.url,
				i.original,
				i.title,
				strip_priv_pcompare(i.content, p.permission, min_manipulate_permission),
				CASE WHEN p.permission >= min_manipulate_permission THEN i.content_raw ELSE '' END
			-- INTO id_out, gallery_out, permission_out, permission_broker_out, url_out, title_out, content_out, content_raw_out
			FROM "image" as i
			INNER JOIN permission as p
			ON p.permission_broker = i.permission
			AND p."character" = character_id
			AND p.permission >= min_view_permission
			WHERE i.id = image_id;
		else
			RETURN QUERY SELECT
				i.id,
				i.gallery,
				0,
				i.permission,
				i.url,
				i.original,
				i.title,
				strip_priv_pcompare(i.content, 0, 0),
				i.content_raw
			-- INTO id_out, gallery_out, permission_out, permission_broker_out, url_out, title_out, content_out, content_raw_out
			FROM "image" as i
			WHERE i.id = image_id;
		end if;
	end;
$$;

CREATE OR REPLACE FUNCTION appointment_view (user_id_p integer, group_id integer, character_id_p integer, appointment_id integer, OUT appointment_id_out integer, OUT "date_out" text, OUT elected_OUT boolean, OUT electorates_out text) RETURNS RECORD
	LANGUAGE plpgsql
	AS $$
	BEGIN
		SELECT id, "date", elected, (SELECT string_agg(ae."user"::text, ',') FROM appointment_electorate AS ae WHERE ae.appointment = a.id)
		INTO appointment_id_out, date_out, elected_OUT, electorates_out
		FROM appointment AS a
		WHERE a."group" = group_id
		AND a.id=appointment_id;
	END;
$$;
