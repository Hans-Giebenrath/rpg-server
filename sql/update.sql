\c rpg;

CREATE OR REPLACE FUNCTION permission_update (user_id_p integer, group_id integer, master_id integer, permission_broker_id integer, slave_id integer, granted_permission integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		master_permission integer;
	BEGIN
		if master_id = slave_id then
			-- this is rather senseless
			RAISE NOTICE '[permission_update] master_id = slave_id (% = %) for pb: %, granted_permission: %', master_id, slave_id, permission_broker_id, granted_permission;
			return;
		end if;
		-- master - group integrity is safe.
		if not exists (select 1 from "character" as c where c.id = slave_id and c."group" = group_id) then
			raise exception '[permission_update] Slave (%) not in group (%) as master (%).', slave_id, group_id, master_id;
		end if;

		if not exists (select 1 from "permission_broker" as pb where pb.id = permission_broker_id and pb."group" = group_id) then
			raise exception '[permission_update] Permission broker (%) not in specified group (%).', permission_broker_id, group_id;
		end if;

		if master_id is not null then
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			-- group check not necessary, as master should just have permissions in his own group
			SELECT p.permission INTO master_permission FROM "permission" as p WHERE p.permission_broker=permission_broker_id AND p."character"=master_id;
			if master_permission is null OR master_permission < min_manipulate_permission OR master_permission < granted_permission then
				RAISE EXCEPTION '[permission_update] Master has no permission to alter slave''s permission. (master_permission: %, min_manipulate_permission %, granted_permission %, permission_broker_id %, paster_id %, slave_id %)', master_permission, min_manipulate_permission, granted_permission, permission_broker_id, master_id, slave_id;
			end if;
		else
			-- no check required.
			null;
		end if;

		UPDATE permission SET permission=granted_permission WHERE permission_broker=permission_broker_id AND "character"=slave_id;
		-- TODO maybe do check with diagnostics, if row was affected

		INSERT INTO permission (permission, permission_broker, "character") SELECT granted_permission, permission_broker_id, slave_id
			   WHERE NOT EXISTS (SELECT 1 FROM permission WHERE permission_broker=permission_broker_id AND "character"=slave_id);
	END;
$$;

CREATE OR REPLACE FUNCTION permission_bulk_update (user_id_p integer, group_id integer, master_id integer, permission_broker_id integer, user_permission integer[][]) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
		master_permission integer;
		pair integer[];
	BEGIN
		if user_permission is not null then
			FOREACH pair SLICE 1 IN ARRAY user_permission 
			LOOP
				perform permission_update (user_id_p, group_id, master_id, permission_broker_id, pair[1], pair[2]);
			END LOOP;
		end if;
	END;
$$;
