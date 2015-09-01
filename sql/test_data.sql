\c rpg;

DO $$
DECLARE
	hans_id integer;
	peter_id integer;
	ulf_id integer;
	karl_id integer;
	sven_id integer;
	richard_id integer;
	alex_id integer;
	lisi_id integer;
	
	gr_hans_id integer;
	gr_ulf_id integer;
	gr_hans_id_gbik integer;
	gr_ulf_id_gbik integer;

	hans_char_id integer;
	peter_char_id integer;
	ulf_char_id integer;
	karl_char_id integer;
	richard_char_id integer;
	sven_char_id integer;
	alex_char_id integer;
	lisi_char_id integer;

	hans_char_gal_id integer;
	peter_char_gal_id integer;
	sven_char_gal_id integer;
	ulf_gr_gal_id integer;

	hans_image_id integer;
	sven_image_id integer;
	peter_image_id integer;
	ulf_image_id integer;

	hans_history_id integer;

	gr_ulf_wiki1_id integer;
	gr_ulf_wiki2_id integer;
	gr_ulf_wiki3_id integer;
	gr_ulf_wiki4_id integer;
	p integer;
BEGIN
	SELECT register ('hans', 'asd', 'h@ns.de') INTO hans_id;
	SELECT register ('peter', 'asd', 'pe@e.r') INTO peter_id;
	SELECT register ('ulf', 'asd', 'u@l.d') INTO ulf_id;
	SELECT register ('karl', 'asd', 'k@r.l') INTO karl_id;
	SELECT register ('richard', 'asd', 'ri@har.d') INTO richard_id;
	SELECT register ('sven', 'asd', 's@v.en') INTO sven_id;
	SELECT register ('alex', 'asd', 'a@le.x') INTO alex_id;
	SELECT register ('lisi', 'asd', 'l@s.i') INTO lisi_id;
	PERFORM activate_user (hans_id);
	PERFORM activate_user (peter_id);
	PERFORM activate_user (ulf_id);
	PERFORM activate_user (karl_id);
	PERFORM activate_user (richard_id);
	PERFORM activate_user (sven_id);
	PERFORM activate_user (alex_id);
	PERFORM activate_user (lisi_id);

	SELECT * FROM group_create (hans_id, null, null, 'Runde 1') INTO gr_hans_id, gr_hans_id_gbik;
	RAISE NOTICE 'gr_hans_id: %, gr_hans_id_gbik: %', gr_hans_id, gr_hans_id_gbik;
	SELECT * FROM character_create (hans_id, gr_hans_id, null, peter_id, 'PeterChar') INTO peter_char_id;
	SELECT * FROM character_create (hans_id, gr_hans_id, null, ulf_id, 'UlfChar') INTO ulf_char_id;
	SELECT * FROM character_create (hans_id, gr_hans_id, null, karl_id, 'KarlChar') INTO karl_char_id;

	SELECT * FROM group_create (ulf_id, null, null, 'Runde 2') INTO gr_ulf_id, gr_ulf_id_gbik;
	SELECT * FROM character_create (ulf_id, gr_ulf_id, null, richard_id, 'RichardChar') INTO richard_char_id;
	SELECT * FROM character_create (ulf_id, gr_ulf_id, null, sven_id, 'SvenChar') INTO sven_char_id;
	SELECT * FROM character_create (ulf_id, gr_ulf_id, null, hans_id, 'HansChar') INTO hans_char_id;
	-- PERFORM group_gamemaster_add (null, gr_ulf_id, null, karl_id);

	SELECT * FROM character_gallery_create_with_permission (hans_id, gr_ulf_id, hans_char_id, 'text1', 'content1', 'content1raw', ARRAY[[sven_char_id,300],[richard_char_id,0]]) INTO hans_char_gal_id, p;

	SELECT * FROM image_create_with_permission (hans_id, gr_ulf_id, hans_char_id, hans_char_gal_id, 'title1', 'cont1', 'cont1raw', 'url1', ARRAY[[sven_char_id,300],[richard_char_id,0]]) INTO hans_image_id, p;

	SELECT * FROM image_create_with_permission (sven_id, gr_ulf_id, sven_char_id, hans_char_gal_id, 'title2', 'cont2', 'cont2raw', 'url2', ARRAY[[hans_char_id, 500],[richard_char_id,0]]) INTO sven_image_id, p;

	SELECT * FROM character_gallery_create_with_permission (hans_id, gr_ulf_id, hans_char_id, 'text2', 'content1', 'content1raw', ARRAY[[sven_char_id,300],[richard_char_id,0]]) INTO hans_char_gal_id, p;

	SELECT * FROM image_create_with_permission (hans_id, gr_ulf_id, hans_char_id, hans_char_gal_id, 'title3', 'cont1', 'cont1raw', 'url3', ARRAY[[sven_char_id,300],[richard_char_id,0]]) INTO hans_image_id, p;

	SELECT * FROM image_create_with_permission (sven_id, gr_ulf_id, sven_char_id, hans_char_gal_id, 'title4', 'cont2', 'cont2raw', 'url4', ARRAY[[hans_char_id, 500],[richard_char_id,0]]) INTO sven_image_id, p;

	SELECT * FROM character_gallery_create_with_permission (peter_id, gr_hans_id, peter_char_id, 'text2', 'content1', 'content1raw', ARRAY[[ulf_char_id,300],[karl_char_id,0]]) INTO peter_char_gal_id, p;

	SELECT * FROM image_create_with_permission (peter_id, gr_hans_id, peter_char_id, peter_char_gal_id, 'title3', 'cont1', 'cont1raw', 'url5', ARRAY[[ulf_char_id,300],[karl_char_id,0]]) INTO peter_image_id, p;

	SELECT * FROM image_create_with_permission (peter_id, gr_hans_id, peter_char_id, peter_char_gal_id, 'title4', 'cont2', 'cont2raw', 'url6', ARRAY[[ulf_char_id,300],[karl_char_id,0]]) INTO peter_image_id, p;

	SELECT * FROM character_gallery_create_with_permission (sven_id, gr_ulf_id, sven_char_id, 'text2', 'content1', 'content1raw', ARRAY[[hans_char_id,300],[richard_char_id,0]]) INTO sven_char_gal_id, p;

	SELECT * FROM image_create_with_permission (sven_id, gr_ulf_id, sven_char_id, sven_char_gal_id, 'title3', 'cont1', 'cont1raw', 'url7', ARRAY[[hans_char_id,300],[richard_char_id,0]]) INTO sven_image_id, p;

	SELECT * FROM image_create_with_permission (sven_id, gr_ulf_id, sven_char_id, sven_char_gal_id, 'title4', 'cont2', 'cont2raw', 'url8', ARRAY[[hans_char_id,300],[richard_char_id,0]]) INTO sven_image_id, p;

	-- PERFORM history_entri

	DECLARE
		tags_1 text[] := ARRAY['eins', 'zwei', 'drei', 'vier'];
		tags_2 text[] := ARRAY['drei', 'vier', 'fünf', 'sechs'];
		tags_3 text[] := ARRAY['zehn', 'elf', 'zwölf', 'dreizehn'];
		tags_4 text[] := ARRAY['zwölf', 'dreizehn', 'vierzehn', 'fünfzehn'];
		wiki12_id integer;
		wiki34_id integer;
		CPerms integer[][];
	BEGIN
		RAISE NOTICE '[wiki] Start inserting wikis.';
		SELECT ARRAY[character_id_out, 100] FROM group_characters_list(null, gr_hans_id, null) INTO CPerms;
	    with data as (select '{' || character_id_out || ',100}' AS el FROM group_characters_list(null, gr_hans_id, null)) select '{' || string_agg(el, ',') || '}' FROM data INTO CPerms;	
		SELECT * FROM wiki_create_with_permission (null, gr_hans_id, null, 'wiki12_title', 'wiki12_slug', 'short', 'short_raw', 'cont', 'cont_raw', tags_1, CPerms) INTO wiki12_id, p;

		RAISE NOTICE '[wiki] First wiki inserted.';
		-- SELECT * FROM wiki;
		-- SELECT * FROM tag;
		-- SELECT * FROM tag_nm;

		CPerms[1][2] := 500;
		SELECT * FROM wiki_create_with_permission (null, gr_hans_id, CPerms[1][1], 'wiki34_title', 'wiki34_slug', 'short', 'short_raw', 'cont', 'cont_raw', tags_3, CPerms) INTO wiki34_id, p;

		RAISE NOTICE '[wiki] Second wiki inserted.';
		-- SELECT * FROM wiki;
		-- SELECT * FROM tag;
		-- SELECT * FROM tag_nm;

		PERFORM wiki_tag_edit (wiki12_id, tags_2);
		RAISE NOTICE '[wiki] First wiki edited.';
		-- SELECT * FROM wiki;
		-- SELECT * FROM tag;
		-- SELECT * FROM tag_nm;

		PERFORM wiki_tag_edit (wiki34_id, tags_4);
		RAISE NOTICE '[wiki] Second wiki edited.';
		-- SELECT * FROM wiki;
		-- SELECT * FROM tag;
		-- SELECT * FROM tag_nm;
	END;
END
$$;
