\c rpg;

CREATE OR REPLACE FUNCTION user_delete (user_id integer, group_id_p integer, character_id_p integer) RETURNS table (url text)
	LANGUAGE plpgsql
	AS $$
	BEGIN
		CREATE TEMP TABLE Gallery_Broker_Ids (gallery_broker)
		AS
		SELECT c."gallery"
		FROM "character" AS c
		WHERE c."user" = user_id;

		CREATE TEMP TABLE Old_Images (permission_broker, url) ON COMMIT DROP
		AS
		SELECT i.permission, i.url FROM "image" AS i
		INNER JOIN "gallery" AS g
		ON g.id=i."gallery"
		INNER JOIN Gallery_Broker_Ids AS gbids
		ON gbids.gallery_broker = g."gallery";



		CREATE TEMP TABLE Old_Galleries (permission_broker) ON COMMIT DROP
		AS
		SELECT permission FROM "gallery" AS g
		INNER JOIN Gallery_Broker_Ids AS gbids
		ON gbids.gallery_broker = g."gallery";

		DELETE FROM permission_broker AS pb
		USING Old_Images AS oi
		WHERE pb.id = oi.permission_broker;

		DELETE FROM permission_broker AS pb
		USING Old_Galleries AS og
		WHERE pb.id = og.permission_broker;

		DELETE FROM gallery_broker AS gb
		USING Gallery_Broker_Ids AS gbids
		WHERE gb.id = gbids.gallery_broker;

		DELETE FROM "user" WHERE id = user_id;

		RETURN QUERY
			(SELECT DISTINCT oi.url FROM Old_Images AS oi)
			UNION
			(WITH grps AS (
				SELECT count(*) AS cnt, g."id"
				FROM "group" as g
				INNER JOIN "gamemaster" AS gm
				ON gm."user" = user_id
				AND gm."group" = g.id
				GROUP BY g."id"
			)
			SELECT group_delete (user_id, null, null, g.id)
			FROM grps AS g
			WHERE g.cnt = 1);

	END;
$$;

CREATE OR REPLACE FUNCTION group_delete (user_id integer, group_id_p integer, character_id_p integer, target_group integer) RETURNS table (url text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		gallery_broker_old integer;
	BEGIN
		IF NOT EXISTS (
			SELECT 1 FROM "gamemaster" as g
			WHERE g."user" = user_id
			AND g."group" = target_group
			) THEN
			RAISE EXCEPTION '[group_delete] You (%) are not gamemaster of the group (%)', user_id, target_group;
		END IF;

		-- Each gm can kick the other gms, so this check just prevents group deletion by some seconds.
		-- Also, as this i all based on trust, this check is not useful.
		-- IF (WITH data AS (
				-- SELECT 1 FROM "gamemaster" AS g
				-- WHERE g."group" = target_group
				-- LIMIT 2 )
			-- SELECT count(1) FROM data) > 1 THEN
			-- RAISE EXCEPTION '[group_delete] There is still another gm in this group (%)', user_id, target_group;
		-- END IF;

		CREATE TEMP TABLE Gallery_Broker_Ids (gallery_broker) ON COMMIT  DROP
		AS
		SELECT c."gallery"
		FROM "character" AS c
		WHERE c."group" = target_group;

		SELECT g.gallery INTO gallery_broker_old
		FROM "group" AS g
		WHERE g.id = target_group;

		INSERT INTO Gallery_Broker_Ids (gallery_broker) VALUES (gallery_broker_old);

		CREATE TEMP TABLE Old_Images (permission_broker, url) ON COMMIT DROP
		AS
		SELECT i.permission, i.url FROM "image" AS i
		WHERE "group" = target_group;

		CREATE TEMP TABLE Old_Permission_Broker (permission_broker) ON COMMIT DROP
		AS
		(SELECT permission FROM "gallery" AS g
		INNER JOIN Gallery_Broker_Ids AS gbids
		ON gbids.gallery_broker = g."gallery")
		UNION
		(SELECT permission_broker FROM Old_Images);

		WITH data AS (DELETE FROM "wiki" WHERE "group" = target_group RETURNING permission)
		INSERT INTO Old_Permission_Broker
		SELECT * FROM data;

		WITH data AS (DELETE FROM "history" WHERE "group" = target_group RETURNING permission)
		INSERT INTO Old_Permission_Broker
		SELECT * FROM data;

		DELETE FROM permission_broker AS pb
		USING Old_Permission_Broker AS opb
		WHERE pb.id = opb.permission_broker;

		DELETE FROM gallery_broker AS gb
		USING Gallery_Broker_Ids AS gbids
		WHERE gb.id = gbids.gallery_broker;

		RETURN QUERY SELECT DISTINCT oi.url FROM Old_Images AS oi;
	END;
$$;

CREATE OR REPLACE FUNCTION gm_delete (user_id integer, group_id integer, character_id integer, target_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		deleted_rows integer;
	BEGIN
		IF target_id = user_id THEN
			RAISE EXCEPTION '[gm_delete] You cannot delete yourself from group.';
		END IF;

		DELETE FROM gamemaster WHERE "user"=target_id AND "group"=group_id;
		GET DIAGNOSTICS deleted_rows = ROW_COUNT;

		IF deleted_rows = 0 THEN
			RAISE EXCEPTION '[gm_delete] The selected user is no gm of your group.';
		END IF;
	END;
$$;

-- As a gamemaster creates a character, only the gamemaster should be able to delete it.
CREATE OR REPLACE FUNCTION character_delete (user_id integer, group_id_p integer, character_id_p integer, target_id integer) RETURNS table (url text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		gallery_broker_old integer;
	BEGIN
		IF NOT check_gm_of_char(user_id, target_id) THEN
			RAISE EXCEPTION '[character_delete] You (%) are not gamemaster of the characters (%) group', user_id, target_id;
		END IF;

		SELECT c.gallery INTO gallery_broker_old
		FROM "character" AS c
		WHERE c.id = target_id;

		CREATE TEMP TABLE Old_Images (permission_broker, url) ON COMMIT DROP
		AS
		SELECT i.permission, i.url FROM "image" AS i
		INNER JOIN "gallery" AS g
		ON g.id=i."gallery"
		AND g."gallery" = gallery_broker_old;

		CREATE TEMP TABLE Old_Galleries (permission_broker) ON COMMIT DROP
		AS
		SELECT permission FROM "gallery" AS g
		WHERE g."gallery" = gallery_broker_old;

		DELETE FROM permission_broker AS pb
		USING Old_Images AS oi
		WHERE pb.id = oi.permission_broker;

		DELETE FROM permission_broker AS pb
		USING Old_Galleries AS og
		WHERE pb.id = og.permission_broker;

		DELETE FROM gallery_broker WHERE id =gallery_broker_old;

		RETURN QUERY SELECT DISTINCT oi.url FROM Old_Images AS oi;
	END;
$$;

-- as images should be stored in directories being named <gallery_id>, we simply can delete the whole directory and forget about first retrieving all imageas.
CREATE OR REPLACE FUNCTION gallery_delete (user_id_p integer, group_id integer, character_id integer, gallery_id integer) RETURNS table (url text)
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_delete_permission integer;
		permission_value integer;
		permission_broker_old integer;
	BEGIN
		SELECT check_gallery_permission_broker(group_id, gallery_id) INTO permission_broker_old;

		if character_id is not null then
			SELECT config_min_delete_permission () INTO min_delete_permission;

			IF NOT check_gallery_permission(character_id, gallery_id, min_delete_permission) then
				raise exception '[gallery_delete] Deleting of image not allowed.';
			end if;
		end if;

		CREATE TEMP TABLE Old_Images (permission_broker, url) ON COMMIT DROP
		AS
		SELECT i.permission, i.url FROM "image" AS i
		WHERE i.gallery = gallery_id;

		DELETE FROM permission_broker AS pb
		USING Old_Images AS oi
		WHERE pb.id = oi.permission_broker;

		DELETE FROM permission_broker
		WHERE id = permission_broker_old;

		RETURN QUERY SELECT DISTINCT oi.url FROM Old_Images AS oi;
	END;
$$;

CREATE OR REPLACE FUNCTION image_delete (user_id_p integer, group_id integer, character_id integer, image_id integer, OUT image_url_out text) RETURNS text
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_delete_permission integer;
		permission_broker_old integer;
	BEGIN
		SELECT check_image_permission_broker(group_id, image_id) INTO permission_broker_old;
		
		if character_id is not null then
			SELECT config_min_delete_permission () INTO min_delete_permission;

			IF NOT check_image_permission(character_id, image_id, min_delete_permission) then
				raise exception '[image_delete] Deleting of image not allowed.';
			end if;
		end if;

		SELECT i.url INTO image_url_out
		FROM "image" AS i
		WHERE i.id = image_id;

		DELETE FROM permission_broker
		WHERE id = permission_broker_old;
	END;
$$;

CREATE OR REPLACE FUNCTION wiki_delete (user_id_p integer, group_id integer, character_id integer, wiki_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_delete_permission integer;
		permission_broker_old integer;
	BEGIN
		SELECT check_wiki_permission_broker(group_id, wiki_id) INTO permission_broker_old;

		if character_id is not null then
			SELECT config_min_delete_permission () INTO min_delete_permission;

			IF NOT check_wiki_permission(character_id, wiki_id, min_delete_permission) then
				raise exception '[wiki_delete] Deleting of wiki not allowed.';
			end if;
		end if;

		DELETE FROM permission_broker where id = permission_broker_old;
		-- NOTE This is not necessary, as deletion of permission_broker also deletes wiki.
		-- DELETE FROM wiki WHERE id=wiki_id AND "group" = group_id;
	END;
$$;

CREATE OR REPLACE FUNCTION history_delete (user_id_p integer, group_id integer, character_id integer, history_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		permission_broker_old integer;
	BEGIN
		IF character_id IS NOT NULL THEN
			RAISE exception '[history_delete] Only gms are allowed to delete the history.';
		END IF;
		-- no further min_delete_permission, as we are gm

		SELECT check_history_permission_broker(group_id, history_id) INTO permission_broker_old;

		DELETE FROM permission_broker where id = permission_broker_old;
		-- NOTE This is not necessary, as deletion of permission_broker also deletes wiki.
		-- DELETE FROM history WHERE id=history_id;
	END;
$$;

-- this deletes ALL appointments of a group
-- single deletion is kinda useless
CREATE OR REPLACE FUNCTION appointment_delete (user_id_p integer, group_id integer, character_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	BEGIN
		IF character_id IS NOT NULL THEN
			RAISE EXCEPTION '[appointment_delete] Only gamemasters are allowed to clear the appointments.';
		END IF;

		DELETE FROM appointment WHERE "group" = group_id;
	END;
$$;

