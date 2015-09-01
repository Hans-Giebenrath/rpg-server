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

CREATE OR REPLACE FUNCTION user_edit (user_id integer, group_id_p integer, character_id_p integer, password_old text, name_new text, password_new text, email_new text) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		password_cur text;
	BEGIN
		-- doesn't care wether "user" or "registered"
		SELECT u.password INTO password_cur
		FROM "user" as u
		WHERE u."id"=user_id;

		IF password_cur <> crypt(password_old, password_cur) THEN
			RAISE EXCEPTION '[user_edit] Your old password is not correct.';
		END IF;

		-- TODO this could be more beautiful with coalesce
		if email_new is not null then
			if password_new is not null then
				UPDATE "user" SET name = name_new, password = crypt(password_new, gen_salt('bf', 8)), email = email_new WHERE id = user_id;
			else
				UPDATE "user" SET name = name_new, email = email_new WHERE id = user_id;
			END IF;
		else
			if password_new is not null then
				UPDATE "user" SET name = name_new, password = crypt(password_new, gen_salt('bf', 8)) WHERE id = user_id;
			else
				UPDATE "user" SET name = name_new WHERE id = user_id;
			END IF;
		end if;
		RETURN;
	END;
$$;
		
-- Editing group name is neither really useful, nor is it worth the effort.
-- If there is a special demand, I can do it by hand. Otherwise, maybe there is
-- a too frequent update, which fucks up the users.
-- CREATE OR REPLACE FUNCTION group_edit (user_id_p integer, group_id integer, character_id integer, name_new text) RETURNS void
	-- LANGUAGE plpgsql
	-- AS $$
	-- BEGIN
		-- -- if the character_id is null, then the user is currently gamemaster.
		-- -- Otherwise, he is a player and not allowed to change the group's name.

		-- IF character_id IS NOT NULL THEN
			-- RAISE EXCEPTION '[group_edit] You are not gamemaster of the group.';
		-- END IF;

		-- UPDATE "group" SET name = name_new WHERE id=group_id;
		-- RETURN;
	-- END;
-- $$;

-- it is not possible for the gm to edit the text. Still send char_to_edit, as we need it for websockets.
CREATE OR REPLACE FUNCTION character_edit (user_id_p integer, group_id_p integer, character_id integer, char_to_edit integer, name_new text, content_new text, content_raw_new text, short_new text, short_raw_new text) RETURNS void
	LANGUAGE plpgsql
	AS $$
	BEGIN
		if character_id <> char_to_edit then
			raise exception '[character_edit] You can only edit your own character.';
		End if;

		UPDATE "character" SET
			name = name_new,
			content = content_new,
			content_raw = content_raw_new,
			short = short_new,
			short_raw = short_raw_new
		WHERE id=character_id;
		RETURN;
	END;
$$;

-- new_owner is a user_id
-- Only the gamemaster has the ownership power over the characters.
-- Also, this way users cannot mistakenly do stuff wrong.
-- old_owner is a user_id and used for the websocket, as we also
-- have to inform the old user.
CREATE OR REPLACE FUNCTION character_owner_edit (user_id integer, group_id integer, character_id_p integer, passed_character integer, new_owner integer, old_owner integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	BEGIN
		IF
			-- NOT check_user_owns_char(user_id, passed_character)
			-- AND
			NOT check_gm_of_char(user_id, passed_character)
		THEN
			RAISE EXCEPTION '[character_owner_edit] You (%) are neither the owner of the character (%) nor its gamemaster.', user_id, passed_character;
		END IF;

		UPDATE "character" SET "user"=new_owner WHERE "id" = passed_character;
	END;
$$;

-- well, the character_id_p is just for a more easy websocket stuff
CREATE OR REPLACE FUNCTION character_set_image (user_id_p integer, group_id integer, character_id integer, character_id_p integer, image_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		gallery_id integer;
		image_url text;
		permission_value_image integer;
		permission_value_gallery integer;
		min_manipulate_permission integer;
	BEGIN
		IF character_id <> character_id_p THEN
			raise exception '[character_set_image] The applied character_id is not yours.';
		END IF;

		IF not exists (
			SELECT 1 from image as i
			INNER JOIN "gallery" as g
			ON g.id = i.gallery
			INNER JOIN "character" as c
			ON c.gallery = g.gallery
			AND c.id = character_id
			WHERE i.id = image_id
			AND i."group" = group_id) then
			raise exception '[character_set_image] Image (%) is not part of your (%) galleries.', image_id, character_id;
		end if;

		SELECT i.url INTO image_url
		FROM image as i
		WHERE i.id=image_id;

		UPDATE "character" SET image=image_url WHERE id=character_id;
	END;
$$;

-- do not discriminate between character gallery and group gallery, as updating always checks permission the same way.
-- NOTE permission_broker_p is required for automatic push of permissions to clients in websocket.adb
CREATE OR REPLACE FUNCTION gallery_edit (user_id_p integer, group_id integer, character_id integer, gallery_id integer, title_new text, content_new text, content_raw_new text, user_permission integer[][], permission_broker_p integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		permission_broker integer;
	BEGIN
		SELECT check_gallery_permission_broker(group_id, gallery_id) INTO permission_broker;

		IF permission_broker <> permission_broker_p THEN
			raise exception '[gallery_edit] The provided permission_broker is wrong. (Supplied: %, Correct: %)', permission_broker_p, permission_broker;
		END IF;

		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			IF NOT check_gallery_permission(character_id, gallery_id, min_manipulate_permission) then
				raise exception '[gallery_update] Updating of gallery information not allowed.';
			end if;
		end if;

		Update gallery SET title=title_new, content=content_new, content_raw=content_raw_new WHERE id=gallery_id;

		PERFORM permission_bulk_update(user_id_p, group_id, character_id, permission_broker, user_permission);
	END;
$$;

-- do not discriminate between character gallery and group gallery, as updating always checks permission the same way.
-- updating of image url (say, the actual image) is fairly nonesense and leads to problems with filesystemstuff. Also I do not want to implement it.
-- NOTE permission_broker_p is required for automatic push of permissions to clients in websocket.adb
CREATE OR REPLACE FUNCTION image_edit (user_id_p integer, group_id integer, character_id integer, image_id integer, title_new text, content_new text, content_raw_new text, user_permission integer[][], permission_broker_p integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		permission_broker integer;
	BEGIN
		SELECT check_image_permission_broker(group_id, image_id) INTO permission_broker;

		IF permission_broker <> permission_broker_p THEN
			raise exception '[image_edit] The provided permission_broker is wrong. (Supplied: %, Correct: %)', permission_broker_p, permission_broker;
		END IF;

		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			IF NOT check_image_permission(character_id, image_id, min_manipulate_permission) then
				raise exception '[image_edit] Updating of image information not allowed.';
			end if;
		end if;

		UPDATE image as i SET title=title_new, content=content_new, content_raw=content_raw_new WHERE i.id=image_id;

		PERFORM permission_bulk_update(user_id_p, group_id, character_id, permission_broker, user_permission);
	END;
$$;

CREATE OR REPLACE FUNCTION image_order_edit (user_id_p integer, group_id integer, character_id integer, gallery_id integer, image_order integer[][]) returns void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
	BEGIN
		PERFORM check_gallery_permission_broker(group_id, gallery_id);

		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			IF NOT check_gallery_permission(character_id, gallery_id, min_manipulate_permission) then
				raise exception '[image_create] Permission not sufficient for you (%).', character_id;
			end if;
		end if;

		CREATE TEMP TABLE New_Order (id, "order") ON COMMIT DROP AS
			SELECT a[1], a[2] FROM reduce_dim(image_order) AS a;

		UPDATE image AS i
		SET "order" = no."order"
		FROM New_Order AS no
		WHERE i.id = no.id
		AND i.gallery = gallery_id; -- make sure, that only images of the same gallery are updated

		DROP TABLE New_Order;
	END;
$$;

-- do not discriminate between character gallery and group gallery, as updating always checks permission the same way.
CREATE OR REPLACE FUNCTION gallery_set_image (user_id_p integer, group_id integer, character_id integer, gallery_id_p integer, image_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		gallery_id integer;
		image_url text;
		permission_value_image integer;
		permission_value_gallery integer;
		min_manipulate_permission integer;
	BEGIN
		PERFORM check_image_permission_broker(group_id, image_id);

		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			IF NOT check_image_permission(character_id, image_id, min_manipulate_permission) then
				raise exception '[gallery_set_image] You (%) are not allowed to access this image (%).', character_id, image_id;
			end if;

			IF NOT check_gallery_permission(character_id, gallery_id_p, min_manipulate_permission) then
				raise exception '[gallery_set_image] You (%) are not allowed to access this gallery (%).', character_id, gallery_id_p;
			end if;
		else
			-- NOTE this really is commented out.
			-- for gms, we have to get the gallery_id seperately
			-- SELECT i.gallery, i.url INTO gallery_id, image_url
			-- FROM image as i
			-- WHERE i.id=image_id;
			null;
		end if;

		-- For a greater modularity, I moved this query down here. So the checks
		-- done in the user part are separate.
		SELECT i.gallery, i.url INTO gallery_id, image_url
		FROM image as i
		WHERE i.id=image_id;

		IF gallery_id <> gallery_id_p THEN
			raise exception '[gallyer_set_image] Wrong gallery_id (%) for image (%) specified. Correct would be %.', gallery_id_p, image_id, gallery_id;
		END IF;

		-- since it is unique, first falsify the old one
		UPDATE image SET is_cover_image=false WHERE gallery=gallery_id AND is_cover_image=true;
		UPDATE image SET is_cover_image=true WHERE id=image_id;
		UPDATE gallery SET image=image_url WHERE id=gallery_id;
	END;
$$;

-- NOTE permission_broker_p is required for automatic push of permissions to clients in websocket.adb
CREATE OR REPLACE FUNCTION history_edit (user_id_p integer, group_id integer, character_id_p integer, history_id integer, title_new text, "date_ingame_new" text, "date_outgame_new" text, short_new text, short_raw_new text, content_new text, content_raw_new text, user_permission integer[][], permission_broker_p integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_broker integer;
	BEGIN
		if character_id_p IS NOT NULL then
			raise exception '[history_edit] Only gamemasters are allowed to change history.';
		end if;

		SELECT check_history_permission_broker(group_id, history_id) INTO permission_broker;

		IF permission_broker <> permission_broker_p THEN
			raise exception '[history_edit] The provided permission_broker is wrong. (Supplied: %, Correct: %)', permission_broker_p, permission_broker;
		END IF;
			
		Update history SET
			title=title_new,
			"date_ingame"=date_ingame_new,
			"date_outgame"=date_outgame_new,
			content=content_new,
			content_raw=content_raw_new,
			short=short_new,
			short_raw=short_raw_new
		WHERE id=history_id;

		PERFORM permission_bulk_update(user_id_p, group_id, character_id_p, permission_broker, user_permission);
				-- permission_bulk_update(user_id_p, group_id, master_id     , permission_broker_id, user_permission integer[][]) RETURNS void
	END;
$$;

CREATE OR REPLACE FUNCTION history_order_edit (user_id_p integer, group_id integer, character_id_p integer, history_order integer[][]) returns void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
	BEGIN
		IF character_id_p IS NOT NULL THEN
			raise exception '[history_order_edit] You are not gm.';
		END IF;

		CREATE TEMP TABLE New_Order (id, "order") ON COMMIT DROP AS
			SELECT a[1], a[2] FROM reduce_dim(history_order) AS a;

		UPDATE history AS h
		SET "order" = no."order"
		FROM New_Order AS no
		WHERE h.id = no.id AND h."group" = group_id; -- make sure, that no history_entries from different group are updated

		DROP TABLE New_Order;
	END;
$$;

CREATE OR REPLACE FUNCTION wiki_tag_edit (wiki_id integer, tags text[]) returns void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		rows_added integer;
	BEGIN
		-- for the tags: we do have:
		-- 1. existing tags = et
		-- 2. old wiki tags = ot
		-- 3. new wiki tags = nt
		-- 4. removable tags = old - new = remt
		-- 5. insertable tags = new - old =inst
		-- steps to insert:
		-- 1. if(#remt > 0) delete from tag_nm (remt)
		-- 2. delete any none referenced tags from tag
		-- 3. insert inst into tag, returning id
		-- 4. encapusle 3. into a insert into tag_nm

		CREATE TEMP TABLE Old_Tags ON COMMIT DROP AS
		(SELECT t.id FROM "tag" as t INNER JOIN tag_nm as nm ON nm.tag = t.id AND nm.wiki = wiki_id
			WHERE t.tag NOT IN (SELECT DISTINCT unnest(tags)));

		GET DIAGNOSTICS rows_added = ROW_COUNT;

		if rows_added > 0 then
			DELETE FROM tag_nm as nm USING old_tags as ot
			WHERE nm.wiki = wiki_id and nm.tag = ot.id;
		END IF;

		CREATE TEMP TABLE New_Tags ON COMMIT DROP AS
		(SELECT DISTINCT unnest(tags))
		EXCEPT
		(SELECT t.tag FROM "tag" as t);

		GET DIAGNOSTICS rows_added = ROW_COUNT;

		if rows_added > 0 then
			RAISE NOTICE '[wiki_tag_edit] Creating new tags.';
			WITH new_ids AS (
				INSERT INTO tag (tag) 
				SELECT * FROM new_tags RETURNING id)
			INSERT INTO tag_nm (tag, wiki) 
				SELECT *, wiki_id FROM new_ids;
		END IF;

		CREATE TEMP TABLE New_Wiki_Tags (tag) ON COMMIT DROP AS
		((SELECT DISTINCT unnest(tags))
		EXCEPT
		(SELECT t.tag FROM "tag" as t INNER JOIN tag_nm as nm ON nm.tag = t.id AND nm.wiki = wiki_id))
		EXCEPT
		(SELECT * FROM New_Tags);

		GET DIAGNOSTICS rows_added = ROW_COUNT;

		if rows_added > 0 then
			RAISE NOTICE '[wiki_tag_edit] Reusing old tags.';
			INSERT INTO tag_nm (tag, wiki) 
				SELECT t.id, wiki_id
				FROM New_Wiki_Tags as nwt
				INNER JOIN tag as t
				ON t.tag = nwt.tag;
		END IF;

		DROP TABLE Old_Tags;
		DROP TABLE New_Tags;
		DROP TABLE New_Wiki_Tags;
	END;
$$;

-- slugs should not be modifiable
-- NOTE permission_broker_p is required for automatic push of permissions to clients in websocket.adb
CREATE OR REPLACE FUNCTION wiki_edit (user_id_p integer, group_id integer, character_id integer, wiki_id integer, title_new text, short_new text, short_raw_new text, content_new text, content_raw_new text, tags text[], user_permission integer[][], permission_broker_p integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		permission_broker integer;
	BEGIN
		SELECT check_wiki_permission_broker(group_id, wiki_id) INTO permission_broker;

		IF permission_broker <> permission_broker_p THEN
			raise exception '[wiki_edit] The provided permission_broker is wrong. (Supplied: %, Correct: %)', permission_broker_p, permission_broker;
		END IF;

		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			IF NOT check_wiki_permission(character_id, wiki_id, min_manipulate_permission) then
				raise exception '[wiki_update] Updating of wiki information not allowed.';
			end if;
		end if;

		UPDATE wiki SET title=title_new, content=content_new, content_raw=content_raw_new, short=short_new, short_raw=short_raw_new WHERE id=wiki_id;
		PERFORM wiki_tag_edit(wiki_id, tags);

		PERFORM permission_bulk_update(user_id_p, group_id, character_id, permission_broker, user_permission);
	END;
$$;

-- action can have 4 values: 0 -> remove from appointment. 1 -> add to appointment. 2 -> unelect appointment. 3 -> elect appointment
CREATE OR REPLACE FUNCTION appointment_edit (user_id integer, group_id integer, character_id integer, appointment_id integer, action integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	BEGIN
		IF action = 0 THEN
			DELETE FROM appointment_electorate WHERE "user" = user_id AND "appointment" = appointment_id;
		ELSIF action = 1 THEN
			IF EXISTS (SELECT 1 FROM appointment_electorate WHERE "appointment" = appointment_id AND "user" = user_id) THEN
				RETURN;
			END IF;

			IF NOT EXISTS (SELECT 1 FROM appointment WHERE id = appointment_id AND "group" = group_id) THEN
				RAISE EXCEPTION '[appointment_edit] You (%) can only apply for an appointment (%) in your group (%).', user_id, appointment_id, group_id;
			END IF;

			INSERT INTO appointment_electorate ("user", "appointment") VALUES (user_id, appointment_id);
		ELSIF action = 2 THEN
			IF character_id IS NOT NULL THEN
				RAISE EXCEPTION '[appointment_edit] Only gamemaster are allowed to unelect an appointment.';
			END IF;
			UPDATE appointment SET elected=false WHERE "group" = group_id AND elected=true;
		ELSIF action = 3 THEN
			IF character_id IS NOT NULL THEN
				RAISE EXCEPTION '[appointment_edit] Only gamemaster are allowed to elect an appointment.';
			END IF;
			-- This will update every appointment, so resets the previously elected.
			UPDATE appointment SET elected=(SELECT id = appointment_id) WHERE "group" = group_id;
		END IF;
	END;
$$;
