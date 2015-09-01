\c rpg;

CREATE OR REPLACE FUNCTION strip_priv_t_f(s text, keep boolean) RETURNS text
	LANGUAGE plpgsql
	AS $$
	BEGIN
		IF keep THEN
			RETURN regexp_replace(s, '\s*?(<p>)?\s*?:priv.(.*?):\s*(</p>)?\s*(.*?)\s*(<p>)?\s*:priv:\s*(</p>)?\s*', '<\2 class="priv">\4</\2>', 'g');
			-- RETURN regexp_replace(s, '\s*(<p>)?\s*:priv.(.*?):\s*(</p>)?\s*(.*)\s*(<p>)?\s*:priv:\s*(</p>)?\s*', '<\2 class="priv">4</\2 I GOT MATCHED>', 'g');
			-- RETURN regexp_replace(s,           :priv.(.*?)              (.*?)           :priv:', '<\1 class="priv">\2</\1>', 'g');
		ELSE
			RETURN regexp_replace(s, '\s*?(<p>)?\s*:priv.*?:.*?:priv:(\s*</p>)?\s*', '', 'g');
		END IF;
	END;
$$;

-- NOTE : entities to strip: short and content. Nothing else (so is "title")
-- this is used for timeline or character
CREATE OR REPLACE FUNCTION strip_priv (s text, owner integer, viewer integer) RETURNS text
	LANGUAGE plpgsql
	AS $$
	BEGIN
		RETURN strip_priv_t_f(s, (viewer IS NULL OR viewer = owner));
	END;
$$;

-- also looks up permission for modifying
CREATE OR REPLACE FUNCTION strip_priv_plookup (s text, owner integer, viewer integer, permission integer) RETURNS text
	LANGUAGE plpgsql
	AS $$
	DECLARE
		min_manipulate_permission integer;
	BEGIN
		IF viewer IS NULL OR viewer = owner THEN
			RETURN strip_priv_t_f(s, TRUE);
		ELSE
			SELECT config_min_manipulate_permission () INTO min_manipulate_permission;
			RETURN strip_priv_t_f(s, (permission >= min_manipulate_permission));
		END IF;
	END;
$$;

-- also looks up permission for modifying
CREATE OR REPLACE FUNCTION strip_priv_pcompare (s text, permission integer, min_manipulate_permission integer) RETURNS text
	LANGUAGE plpgsql
	AS $$
	BEGIN
		RETURN strip_priv_t_f(s, (permission >= min_manipulate_permission));
	END;
$$;

-- also looks up permission for modifying
CREATE OR REPLACE FUNCTION strip_priv_pcompare (s text, owner integer, viewer integer, permission integer, min_manipulate_permission integer) RETURNS text
	LANGUAGE plpgsql
	AS $$
	BEGIN
		IF viewer IS NULL OR viewer = owner THEN
			RETURN strip_priv_t_f(s, TRUE);
		ELSE
			RETURN strip_priv_t_f(s, (permission >= min_manipulate_permission));
		END IF;
	END;
$$;

CREATE OR REPLACE FUNCTION login (name_p text, pw text) RETURNS integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		user_id integer;
	BEGIN
		-- here, it has to be "user", not "registered"
		SELECT u.id INTO user_id
		FROM "user" as u
		WHERE u."name"=name_p
		AND u.password=crypt(pw, u.password);

		if user_id is null then
			RAISE EXCEPTION '[login] You (%) have applied the wrong password.', name_p;
		end if;

		RETURN user_id;
	END;
$$;

CREATE OR REPLACE FUNCTION register (new_name text, new_pw text, new_email text) RETURNS integer
	LANGUAGE plpgsql
	AS $$
	DECLARE
		user_id integer;
	BEGIN
		INSERT INTO  "registered" (name, password, email, pending) VALUES (new_name, crypt(new_pw, gen_salt('bf', 8)), new_email, true) RETURNING id INTO user_id;
		return user_id;
	END;
$$;

CREATE OR REPLACE FUNCTION activate_user (_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	BEGIN
		UPDATE "registered" SET pending=false WHERE id=_id;
	END;
$$;

CREATE OR REPLACE FUNCTION check_role_gm (user_id integer, group_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	BEGIN
		if not exists (
			SELECT 1 
			FROM "gamemaster" as g
			WHERE g."user"=user_id
			AND g."group"=group_id) then
			RAISE EXCEPTION '[check_role_gm] You (%) are not gamemaster of this group (%).', user_id, group_id;
		end if;
	END;
$$;

-- do not resend charactername, as this should already be stored at the client
CREATE OR REPLACE FUNCTION check_role_character (user_id integer, group_id integer, character_id integer) RETURNS void
	LANGUAGE plpgsql
	AS $$
	DECLARE
		ret integer;
	BEGIN
		if not exists (
			SELECT 1
			FROM "character" as c
			WHERE c."user"=user_id
			AND c."group"=group_id
			AND c.id=character_id) then
			RAISE EXCEPTION '[check_role_character] You (%) have no character (%) in this group (%).', user_id, character_id, group_id;
		end if;
	end;
$$;

CREATE OR REPLACE FUNCTION public.reduce_dim(anyarray) RETURNS SETOF anyarray
	LANGUAGE plpgsql IMMUTABLE
	AS $$
	DECLARE
		s $1%type;
	BEGIN
		FOREACH s SLICE 1  IN ARRAY $1 LOOP
			RETURN NEXT s;
		END LOOP;
		RETURN;
	END;
$$;
