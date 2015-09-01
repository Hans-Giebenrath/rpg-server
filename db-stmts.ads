with db; use db;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package db.stmts is
	Statement_Prefix : String := "Stmt_";
	-- type param_values_s is array (Integer range <>) of access String;

	procedure Init (Connection_String_P : String; Number_Of_Connections : Natural);

	type Statement is (
		-- create
		group_create,
		character_create,
		group_gamemaster_add,
		appointment_create,
		character_gallery_create_with_permission,
		group_gallery_create_with_permission,
		image_create_with_permission,
		history_create_with_permission,
		wiki_create_with_permission,
		-- edit
		user_edit,
		character_edit,
		character_owner_edit,
		character_set_image,
		appointment_edit,
		gallery_edit,
		image_edit,
		image_order_edit,
		gallery_set_image,
		history_edit,
		history_order_edit,
		wiki_edit,
		permission_bulk_update,
		permission_update,
		-- delete
		user_delete,
		group_delete,
		gm_delete,
		character_delete,
		appointment_delete,
		gallery_delete,
		image_delete,
		wiki_delete,
		history_delete,
		-- list
		own_characters_list,
		own_gms_list,
		group_characters_list,
		group_gms_list,
		appointment_list,
		gallery_list,
		image_list,
		image_order_list,
		wiki_list,
		tag_list,
		user_list,
		permission_list,
		history_list,
		history_order_list,
		-- view
		user_view,
		group_view,
		appointment_view,
		wiki_view,
		history_view,
		character_view,
		gallery_view,
		image_view,
		-- role
		register,
		login,
		check_role_gm,
		check_role_character
	);

	subtype Ajax_Statement is Statement range group_create .. image_view;

	subtype Notifiable_Statement is Ajax_Statement range group_create .. history_delete;
	subtype Create_Statement is Notifiable_Statement range group_create .. wiki_create_with_permission;
	subtype Edit_Statement is Notifiable_Statement range user_edit .. permission_update;
	subtype Delete_Statement is Notifiable_Statement range user_delete .. history_delete;

	subtype Get_Statement is Ajax_Statement range own_characters_list .. image_view;
	subtype List_Statement is Get_Statement range own_characters_list .. history_order_list;
	subtype View_Statement is Get_Statement range user_view .. image_view;

	subtype Role_Statement is Statement range register .. check_role_character;

	type Statement_Return_Type is (void, single, table);
	subtype Statement_Signature_Width is Natural range 2 .. 13;
	type Statement_Signature_T is
		record
			Parameters : Statement_Signature_Width;
			Return_Width : Natural;
			Return_Type : Statement_Return_Type;
		end record;

	type Statement_Parameters_T is array (Statement) of Statement_Signature_T;
	Statement_Parameters : constant Statement_Parameters_T := (
		-- create
		group_create => (4, 2, single),
		character_create => (5, 2, single),
		group_gamemaster_add => (4, 0, void),
		appointment_create => (4, 1, single),
		character_gallery_create_with_permission => (7, 2, single),
		group_gallery_create_with_permission => (7, 2, single),
		image_create_with_permission => (10, 2, single),
		history_create_with_permission => (11, 2, single),
		wiki_create_with_permission => (11, 2, single),
		-- edit
		user_edit => (7, 0, void),
		character_edit => (9, 0, void),
		character_owner_edit => (6, 0, void),
		character_set_image => (5, 0, void),
		appointment_edit => (5, 0, void),
		gallery_edit => (9, 0, void),
		image_edit => (9, 0, void),
		image_order_edit => (5, 0, void),
		gallery_set_image => (5, 0, void),
		history_edit => (13, 0, void),
		history_order_edit => (4, 0, void),
		wiki_edit => (12, 0, void),
		permission_bulk_update => (5, 0, void),
		permission_update => (6, 0, void),
		-- delete
		user_delete => (3, 1, table),
		group_delete => (4, 1, table),
		gm_delete => (4, 0, void),
		character_delete => (4, 1, table),
		appointment_delete => (3, 0, void),
		gallery_delete => (4, 1, table),
		image_delete => (4, 1, single),
		wiki_delete => (4, 0, void),
		history_delete => (4, 0, void),
		-- list
		own_characters_list => (3, 5, table),
		own_gms_list => (3, 3, table),
		group_characters_list => (3, 8, table),
		group_gms_list => (3, 2, table),
		appointment_list => (3, 4, table),
		gallery_list => (3, 7, table),
		image_list => (4, 9, table),
		image_order_list => (4, 2, table),
		wiki_list => (3, 8, table),
		tag_list => (3, 2, table),
		user_list => (3, 2, table),
		permission_list => (4, 3, table),
		history_list => (3, 9, table),
		history_order_list => (3, 2, table),
		-- view
		user_view => (4, 2, single),
		group_view => (3, 2, single),
		wiki_view => (4, 11, single),
		appointment_view => (4, 4, single),
		history_view => (4, 12, single),
		character_view => (4, 10, single),
		gallery_view => (4, 8, single),
		image_view => (4, 9, single),
		-- role
		register => (3, 0, void), -- as the user has to wait for activation, do not tell the user_id
		login => (2, 1, single),
		check_role_gm => (2, 0, void),
		check_role_character => (3, 0, void)
	);

	procedure prepare_stmt (stmt_to_prepare : Statement; connection : PGconnection);
	function Execute_Prepared_Scalar (stmt_to_execute : Statement; connection : PGconnection; param_values : param_values_t) return Unbounded_String;
	procedure execute_prepared_void (stmt_to_execute : Statement; connection : PGconnection; param_values : param_values_t);
	function execute_prepared (stmt_to_execute : Statement; connection : PGconnection; param_values : param_values_t) return db.PGresult
		with
			Pre => Statement_Parameters(Stmt_To_Execute).Parameters = Param_Values'Length;

	function Statement_Name (Stmt : Statement) return String;
	Pragma Inline(Statement_Name);
end db.stmts;
