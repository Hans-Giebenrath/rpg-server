\c rpg;

DROP TABLE IF EXISTS "gallery_broker" CASCADE;
CREATE TABLE IF NOT EXISTS "gallery_broker" (
	id serial primary key
);

DROP TABLE IF EXISTS "group" CASCADE;
CREATE TABLE IF NOT EXISTS "group" (
	id serial PRIMARY KEY, -- list (own_gms_list)
	name text NOT NULL UNIQUE CHECK (char_length(name) > 0), -- list (own_gms_list) -- readonly
	gallery integer not null references gallery_broker ON DELETE CASCADE UNIQUE -- list (own_gms_list)
);

DROP TABLE IF EXISTS "permission_broker" CASCADE;
CREATE TABLE IF NOT EXISTS "permission_broker" (
	id serial primary key,
	"group" integer not null references "group"
);

-- no short, as it is not useful.
-- if at any point it will be slower, then maybe group_id can be put here, as it is constant
DROP TABLE IF EXISTS "gallery" CASCADE;
CREATE TABLE IF NOT EXISTS "gallery" (
	id serial primary key, -- list
	permission integer not null references permission_broker ON DELETE CASCADE UNIQUE, -- list
	created timestamp without time zone default now(),
	content text not null DEFAULT '', -- list
	content_raw text not null DEFAULT '', -- view
	title text not null CHECK(char_length(title) > 0), -- list
	-- image is the url to cover image. If this image is deleted, then the url should be cleared, too.
	image text not null DEFAULT '', -- list
	"group" integer not null, -- do this for simplicity - easily check group
	-- uniqueness for gallery-broker is nonesense
	"gallery" integer not null references gallery_broker ON DELETE CASCADE
);

DROP TABLE IF EXISTS "registered" CASCADE;
CREATE TABLE IF NOT EXISTS "registered" (
	id serial PRIMARY KEY,
	name text not null CHECK(char_length(name) > 0),
	email text not null CHECK(char_length(email) > 0), -- required for password stuff
	password text not null,
	pending BOOLEAN DEFAULT TRUE,
	date timestamp without time zone DEFAULT now()
);

CREATE INDEX ON "registered" (name);
CREATE UNIQUE INDEX ON "registered" (lower(name));

DROP VIEW IF EXISTS "user";
CREATE VIEW "user" AS
SELECT id, name, email, password
FROM "registered"
WHERE pending = false;

DROP VIEW IF EXISTS "pending";
CREATE VIEW "pending" AS
SELECT id, name, email
FROM "registered"
WHERE pending = true;

DROP TABLE IF EXISTS "appointment" CASCADE;
CREATE TABLE IF NOT EXISTS "appointment" (
	id serial PRIMARY KEY,
	"date" text NOT NULL CHECK (char_length("date") > 0),
	"group" integer not null references "group" ON DELETE CASCADE,
	"elected" boolean not null default false,
	UNIQUE ("group", "date")
);

DROP TABLE IF EXISTS "appointment_electorate" CASCADE;
CREATE TABLE IF NOT EXISTS "appointment_electorate" (
	"user" integer not null references "registered" ON DELETE CASCADE,
	"appointment" integer not null references "appointment" ON DELETE CASCADE
);

DROP TABLE IF EXISTS "character" CASCADE;
CREATE TABLE IF NOT EXISTS "character" (
	id serial primary key, -- list
	name text not null CHECK(char_length(name) > 0), -- list
	"user" integer references "registered" ON DELETE SET NULL,
	"group" integer not null references "group" ON DELETE CASCADE, -- implicit
	image text not null DEFAULT '', -- list
	gallery integer not null references gallery_broker ON DELETE CASCADE UNIQUE, -- list
	short text not null DEFAULT '', -- list
	short_raw text not null DEFAULT '', -- view
	content text not null DEFAULT '', -- list
	content_raw text not null DEFAULT '', -- view
	-- unique group name is not good, if some characters share the same name
	UNIQUE ("user", "group", "name")
);

DROP TABLE IF EXISTS "gamemaster" CASCADE;
CREATE TABLE IF NOT EXISTS "gamemaster" (
	-- id serial primary key,
	"user" integer not null references "registered" ON DELETE CASCADE,
	"group" integer not null references "group" ON DELETE CASCADE,
	PRIMARY KEY ("user", "group")
);

DROP TABLE IF EXISTS "permission" CASCADE;
CREATE TABLE IF NOT EXISTS "permission" (
	permission_broker integer references permission_broker ON DELETE CASCADE,
	permission integer NOT NULL,
	"character" integer not null references "character" ON DELETE CASCADE,
	UNIQUE (permission_broker, "character")
);

DROP TABLE IF EXISTS "image" CASCADE;
CREATE TABLE IF NOT EXISTS "image" (
	id serial primary key,
	gallery integer not null references "gallery" ON DELETE CASCADE,
	is_cover_image boolean DEFAULT false,
	permission integer not null references permission_broker ON DELETE CASCADE UNIQUE,
	created timestamp without time zone default now(),
	-- do not reference - circular
	"order" integer not null default 0,
	"group" integer not null, -- do this for simplicity - easily check group
	title text not null CHECK(char_length(title) > 0),
	url text not null CHECK(char_length(url) = 16),
	original text not null CHECK(char_length(original) > 0),
	content text not null DEFAULT '',
	content_raw text not null DEFAULT '',
	INDEX("url", original)
);

DROP TABLE IF EXISTS "wiki" CASCADE;
CREATE TABLE IF NOT EXISTS "wiki" (
	id serial primary key, -- list
	"group" integer not null references "group", -- implicit
	permission integer not null references permission_broker ON DELETE CASCADE UNIQUE, -- list
	created timestamp without time zone default now(), -- list
	slug text not null CHECK(char_length(slug) > 0), -- list
	short text not null DEFAULT '', -- list
	short_raw text not null DEFAULT '', -- view
	content text not null DEFAULT '', -- view
	content_raw text not null DEFAULT '', -- view
	title text not null CHECK(char_length(title) > 0), -- list
	UNIQUE("group", slug)
);

-- As the tag-edit procedure is good enough, we can forbid duplicates.
-- but: don't clear. Keep forever, once created
DROP TABLE IF EXISTS "tag" CASCADE;
CREATE TABLE IF NOT EXISTS "tag" (
	id serial primary key,
	tag text not null CHECK(char_length(tag) > 0) unique
);

DROP TABLE IF EXISTS "tag_nm" CASCADE;
CREATE TABLE IF NOT EXISTS "tag_nm" (
	wiki integer not null references "wiki" ON DELETE CASCADE,
	tag integer not null references "tag" ON DELETE CASCADE,
	UNIQUE ( "wiki", "tag")
);

-- history should not be editable by characters,
-- as there may are hidden information
DROP TABLE IF EXISTS "history" CASCADE;
CREATE TABLE IF NOT EXISTS "history" (
	-- add some field for ingame date
	id serial primary key,
	permission integer not null references permission_broker ON DELETE CASCADE UNIQUE,
	created timestamp without time zone default now(),
	-- as each group could use an arbitrary date,
	-- let them enter themselves any format.
	"date_ingame" text not null,
	"date_outgame" text not null,
	-- TODO actually a real date, when they played in the real world, would be cool ...
	-- > a js-understandable text format should do it, I guess.
	-- TODO make history orderable, as the "date" field may is not logically orderable
	"order" integer not null default 0,
	"group" integer not null references "group",
	short text not null DEFAULT '', -- list
	short_raw text not null DEFAULT '', -- view
	content text not null DEFAULT '', -- view
	content_raw text not null DEFAULT '', -- view
	title text not null
);

\ir scheme_view.sql
