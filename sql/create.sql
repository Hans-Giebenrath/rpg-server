\c rpg;

-- NOTE The creator functions generally return some value, but it is highly useless in case of ADA handling websockets.
-- Since on every change, we will inform the group hereof.
CREATE OR REPLACE FUNCTION group_create (user_id integer, group_id_p integer, character_id_p integer, name text, OUT group_id integer, OUT gallery_broker_id integer) RETURNS record
	LANGUAGE plpgsql
	AS $$
	BEGIN
		INSERT INTO gallery_broker DEFAULT VALUES RETURNING id INTO gallery_broker_id;
		INSERT INTO "group" (name, gallery) VALUES (name, gallery_broker_id) RETURNING id INTO group_id;
		INSERT INTO "gamemaster" ("user", "group") VALUES (user_id, group_id);
	END;
$$;

CREATE OR REPLACE FUNCTION group_gamemaster_add (user_id_p integer, group_id integer, character_id_p integer, user_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	BEGIN
		INSERT INTO "gamemaster" ("user", "group") VALUES (user_id, group_id);
	END;
$$;

CREATE OR REPLACE FUNCTION character_create (user_id_p integer, group_id integer, character_id_p integer, user_id integer, name text, OUT character_id integer, OUT gallery_broker_id integer) RETURNS record
	LANGUAGE plpgsql
	AS $$
	BEGIN
		-- allow unactivated users to be added.
		INSERT INTO gallery_broker DEFAULT VALUES RETURNING id INTO gallery_broker_id;
		INSERT INTO "character" (name, gallery, "user", "group") VALUES (name, gallery_broker_id, user_id, group_id) RETURNING id INTO character_id;
		-- TODO copy others permission with big-"and" - so just allowed to watch, what everyone else is allowed to watch
		INSERT INTO permission (permission_broker, permission, "character") 
		WITH data AS (
			SELECT min(dp.permission) as permission, dp.permission_broker as permission_broker
			FROM permission AS dp
			INNER JOIN permission_broker as dpb
			ON dpb."id" = dp.permission_broker
			AND dpb."group" = group_id
			GROUP BY dp.permission_broker)
		SELECT pb.id, COALESCE(data.permission, 0), character_id
		FROM permission_broker as pb
		LEFT OUTER JOIN data
		ON data.permission_broker = pb.id
		WHERE pb."group"=group_id;
	END;
$$;

-- character_id is not allowed to be null
CREATE OR REPLACE FUNCTION character_gallery_create (character_id integer, group_id integer, title text, content text, content_raw text, OUT permission_broker_id integer, OUT gallery_id integer) RETURNS RECORD
	LANGUAGE plpgsql
	AS $$
	DECLARE
		gallery_broker_id integer;
	BEGIN
		SELECT gallery INTO gallery_broker_id FROM "character" WHERE id = character_id;
		INSERT INTO permission_broker ("group") VALUES (group_id) RETURNING id INTO permission_broker_id;
		INSERT INTO gallery (gallery, permission, content, content_raw, title, "group") VALUES (gallery_broker_id, permission_broker_id, content, content_raw, title, group_id) RETURNING id INTO gallery_id;
	END;
$$;

-- after gallery-creation, listen for an incoming gallery with that id and switch immediately
CREATE OR REPLACE FUNCTION character_gallery_create_with_permission (user_id_p integer, group_id integer, character_id integer, title text, content text, content_raw text, user_permission integer[][], OUT gallery_id integer, OUT permission_broker_id integer) RETURNS record
	LANGUAGE plpgsql
	AS $$
	DECLARE
		creator_permission integer;
		pair integer[];
	BEGIN

		select * FROM character_gallery_create(character_id, group_id, title, content, content_raw) into permission_broker_id, gallery_id;
		-- let the gamemaster create a gallery in the user's gallery
		IF character_id IS NOT NULL then 
			SELECT config_creator_permission() INTO creator_permission;
			INSERT INTO permission (permission, permission_broker, "character") VALUES (creator_permission, permission_broker_id, character_id);
		END IF;

		PERFORM permission_bulk_update(user_id_p, group_id, character_id, permission_broker_id, user_permission);
	END;
$$;

CREATE OR REPLACE FUNCTION group_gallery_create (group_id integer, title text, content text, content_raw text, OUT permission_broker_id integer, OUT gallery_id integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		gallery_broker_id integer;
	BEGIN
		SELECT gallery INTO gallery_broker_id FROM "group" WHERE id=group_id;
		INSERT INTO permission_broker ("group") VALUES (group_id) RETURNING id INTO permission_broker_id;
		INSERT INTO gallery (gallery, permission, content, content_raw, title, "group") VALUES (gallery_broker_id, permission_broker_id, content, content_raw, title, group_id) RETURNING id INTO gallery_id;
	END;
$$;

-- after gallery-creation, listen for an incoming gallery with that id and switch immediately
CREATE OR REPLACE FUNCTION group_gallery_create_with_permission (user_id_p integer, group_id integer, character_id integer, title text, content text, content_raw text, user_permission integer[][], OUT gallery_id integer, OUT permission_broker_id integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		creator_permission integer;
		pair integer[];
	BEGIN
		-- no group check required, as group_id and character_id is trusted.
		select * from group_gallery_create(group_id, title, content, content_raw) into permission_broker_id, gallery_id;
		if character_id is not null then 
			SELECT config_creator_permission () INTO creator_permission;
			INSERT INTO permission (permission, permission_broker, "character") VALUES (creator_permission, permission_broker_id, character_id);
		else
			-- do nothing. created by gamemaster.
			null;
		end if;

		PERFORM permission_bulk_update(user_id_p, group_id, character_id, permission_broker_id, user_permission);
	end;
$$;

CREATE OR REPLACE FUNCTION image_create (gallery_id integer, group_id integer, title text, content text, content_raw text, url text, original text, OUT permission_broker_id integer, OUT image_id integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		-- comment_broker_id integer;
		max_order integer;
	BEGIN
		-- INSERT INTO comment_broker DEFAULT VALUES RETURNING id INTO comment_broker_id;
		SELECT COALESCE(max(i."order"), 0) FROM image AS i WHERE i.gallery = gallery_id INTO max_order;
		INSERT INTO permission_broker ("group") VALUES (group_id) RETURNING id INTO permission_broker_id;
		INSERT INTO image (gallery, "group", permission, title, url, original, content, content_raw, "order") VALUES (gallery_id, group_id, permission_broker_id, title, url, original, content, content_raw, max_order) RETURNING id INTO image_id;
	END;
$$;

-- after image-creation, listen for an incoming gallery with that id and switch immediately
CREATE OR REPLACE FUNCTION image_create_with_permission (user_id_p integer, group_id integer, character_id integer, gallery_id integer, title text, content text, content_raw text, url text, original text, user_permission integer[][], OUT image_id integer, OUT permission_broker_id integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		creator_permission integer;
		permission_value integer;
		pair integer[];
	BEGIN
		PERFORM check_gallery_permission_broker(group_id, gallery_id);

		if character_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;

			IF NOT check_gallery_permission(character_id, gallery_id, min_manipulate_permission) then
				raise exception '[image_create] Permission not sufficient for you (%).', character_id;
			end if;
		end if;

		-- Check for character-in-group integrity not required,
		-- as they should be consistently taken out of the database at startup.

		-- contains check for permission to manipulate gallery
		select * FROM image_create (gallery_id, group_id, title, content, content_raw, url, original) INTO permission_broker_id, image_id;
		if character_id is not null then
			SELECT config_creator_permission () INTO creator_permission;
			INSERT INTO permission (permission, permission_broker, "character") VALUES (creator_permission, permission_broker_id, character_id);
		else
			-- do nothing. created by gamemaster.
			null;
		end if;

		PERFORM permission_bulk_update(user_id_p, group_id, character_id, permission_broker_id, user_permission);
	end;
$$;

-- just allowed by gamemaster
CREATE OR REPLACE FUNCTION history_create (group_id integer, title text, "date_ingame" text, "date_outgame" text, short text, short_raw text, content text, content_raw text, OUT permission_broker_id integer, OUT history_id integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_value integer;
		max_order integer;
		-- comment_broker_id integer;
	BEGIN
		-- INSERT INTO comment_broker DEFAULT VALUES RETURNING id INTO comment_broker_id;
		SELECT COALESCE(max(h."order"), 0) FROM history AS h WHERE h."group" = group_id INTO max_order;
		INSERT INTO permission_broker ("group") VALUES (group_id) RETURNING id INTO permission_broker_id;
		INSERT INTO history (permission, title, "date_ingame", "date_outgame", short, short_raw, content, content_raw, "group", "order") VALUES (permission_broker_id, title, "date_ingame", "date_outgame", short, short_raw, content, content_raw, group_id, max_order + 1) RETURNING id INTO history_id;
	END;
$$;

-- history is created by gamemaster. So do not transfer character_id.
-- after history-creation, listen for an incoming gallery with that id and switch immediately
CREATE OR REPLACE FUNCTION history_create_with_permission (user_id_p integer, group_id integer, character_id_p integer, title text, "date_ingame" text, "date_outgame" text, short text, short_raw text, content text, content_raw text, user_permission integer[][], OUT history_id integer, OUT permission_broker_id integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		pair integer[];
	BEGIN
		IF character_id_p IS NOT NULL THEN
			RAISE EXCEPTION '[history_create_with_permission] Only the gamemaster is allowed to create a history entry.';
		END IF;

		select * from history_create (group_id, title, "date_ingame", "date_outgame", short, short_raw, content, content_raw) INTO permission_broker_id, history_id;

		PERFORM permission_bulk_update(user_id_p, group_id, character_id_p, permission_broker_id, user_permission);
	end;
$$;

CREATE OR REPLACE FUNCTION tag_lookup (tag_in text) returns integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		tag_id integer;
	BEGIN
		SELECT id INtO tag_id
		FROM "tag" AS t
		where t.tag = tag_in;

		IF tag_id IS NULL THEN
			INSERT INTO tag (tag) VALUES (tag_in) RETURNING id INTO tag_id;
		END IF;

		return tag_id;
	END;
$$;

CREATE OR REPLACE FUNCTION wiki_create (group_id integer, title text, slug text, short text, short_raw text, content text, content_raw text, tags text[], OUT permission_broker_id integer, OUT wiki_id integer) returns record
	LANGUAGE plpgsql
	AS $$
	DECLARE
		-- comment_broker_id integer;
	BEGIN
		-- INSERT INTO comment_broker DEFAULT VALUES RETURNING id INTO comment_broker_id;
		INSERT INTO permission_broker ("group") VALUES (group_id) RETURNING id INTO permission_broker_id;
		INSERT INTO wiki ("group", permission, slug, short, short_raw, content, content_raw, title) VALUES (group_id, permission_broker_id, slug, short, short_raw, content, content_raw, title) RETURNING id INTO wiki_id;
	END;
$$;

-- after wiki-creation, listen for an incoming gallery with that id and switch immediately
CREATE OR REPLACE FUNCTION wiki_create_with_permission (user_id_p integer, group_id integer, character_id integer, title text, slug text, short text, short_raw text, content text, content_raw text, tags text[], user_permission integer[][], OUT wiki_id integer)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		creator_permission integer;
		permission_broker_id integer;
		pair integer[];
	BEGIN
		-- integrity of character_id and group_id is trusted

		select * from wiki_create (group_id, title, slug, short, short_raw, content, content_raw, tags) INTO permission_broker_id, wiki_id;

		if character_id is not null then
			SELECT config_creator_permission () INTO creator_permission;
			INSERT INTO permission (permission, permission_broker, "character") VALUES (creator_permission, permission_broker_id, character_id);
		else
			-- do nothin. created by gamemaster.
		end if;

		PERFORM wiki_tag_edit (wiki_id, tags);

		PERFORM permission_bulk_update(user_id_p, group_id, character_id, permission_broker_id, user_permission);
	end;
$$;

CREATE OR REPLACE FUNCTION appointment_create (user_id integer, group_id integer, character_id integer, date_in text, OUT appointment_id integer) returns integer
	LANGUAGE plpgsql
	AS $$
	BEGIN
		INSERT INTO appointment ("date", "group") VALUES (date_in, group_id) RETURNING id INTO appointment_id;
		INSERT INTO appointment_electorate ("user", "appointment") VALUES (user_id, appointment_id);
	END;
$$;
