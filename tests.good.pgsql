DROP SCHEMA IF EXISTS tests CASCADE;
CREATE SCHEMA IF NOT EXISTS tests;
SET search_path TO tests;
-- CREATE TABLE moz_foo (id INTEGER PRIMARY KEY AUTOINCREMENT, keyword VARCHAR(32) UNIQUE NOT NULL DEFAULT 'foo');
CREATE TABLE moz_foo (
	id bigint PRIMARY KEY,
	keyword text DEFAULT 'foo' UNIQUE NOT NULL
);
-- INSERT INTO "moz_foo" VALUES(1,'goto');
INSERT INTO moz_foo(id,keyword) VALUES(1,'goto');
-- CREATE TABLE moz_bar (id LONG PRIMARY KEY DEFAULT 0, keyWord LONGVARCHAR UNIQUE DEFAULT NULL);
CREATE TABLE moz_bar (
	id bigint DEFAULT 0 PRIMARY KEY,
	"keyWord" text DEFAULT NULL UNIQUE
);
-- INSERT INTO "moz_bar" VALUES(1,'goto');
INSERT INTO moz_bar(id,"keyWord") VALUES(1,'goto');
-- CREATE TABLE moz_baz (foo DATETIME, bar BLOB, PRIMARY KEY (foo, bar), OnYou INTEGER);
CREATE TABLE moz_baz (
	foo timestamp,
	bar bytea,
	PRIMARY KEY (foo, bar),
	"OnYou" bigint
);
-- INSERT INTO "moz_baz" VALUES('2014-3-8',X'1234', 10);
INSERT INTO moz_baz(foo,bar,"OnYou") VALUES(to_timestamp('2014-3-8'),E'\\x1234',10);
-- CREATE UNIQUE INDEX moz_baz_foo_OnYou ON moz_baz (foo, OnYou);
CREATE UNIQUE INDEX "moz_baz_foo_OnYou" ON moz_baz (foo,"OnYou");
