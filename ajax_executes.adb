with Ada.Strings.Unbounded;
with Util; use Util;
with Gnatcoll.Json; use Gnatcoll.Json;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Deallocation; 
with Db; use Db;
with Db.Stmts; use Db.Stmts;
with Db.Executors; use Db.Executors;

package body Ajax_Executes is
	function execute (Statement_To_Execute : Ajax_Statement; Params : Params_T) return JSON_Value is
		function To_Chars_Ptr(S : String) return chars_ptr is
			(if S = "" then null_ptr else New_String(S));

		Result : JSON_Value := Create_Object;
		type Param_Values_T_Access is access Param_Values_T;
		procedure Free is new Ada.Unchecked_Deallocation(Param_Values_T, Param_Values_T_Access);
		Param_Values : Param_Values_T_Access := new Param_Values_T(1 .. Size_T(Params'Last - Params'First + 4));
	begin
		Param_Values(1) := To_Chars_Ptr(AWS.Session.Get(S, "user_id"));
		Param_Values(2) := To_Chars_Ptr(AWS.Session.Get (S, "group_id"));
		Param_Values(3) := To_Chars_Ptr(AWS.Session.Get (S, "character_id"));
		for I in 4 .. Param_Values'Last loop
			Param_Values(I) := To_Chars_Ptr(Params(Params'First + I - 4));
		end loop;

		case Statement_Parameters(Statement_To_Execute).Return_Type is
			when Stmts.Void =>
				Execute(Param_Values, Statement_To_Execute);
			when Stmts.Single =>
				declare
					RS : Executors.Single(Param_Values, Statement_To_Execute);
					Row : Row_Data_Access;
					Arr : Json_Array := Empty_Array;
				begin
					Row := Execute (RS);
					for I in Row.all'Range loop
						Append(Arr, Create(To_String(Row(I))));
					end loop;
					Result.Set_Field("result", Create(Arr));
				end;
			when Stmts.Table =>
				declare
					RS : Executors.Table (Param_Values, Statement_To_Execute);
					Arr : Json_Array := Empty_Array;
				begin
					for Row of RS loop
						declare
							Arr_Inner : Json_Array := Empty_Array;
						begin
							for I in Row.all'Range loop
								Append(Arr_Inner, Create(To_String(Row(I))));
							end loop;
							Append(Arr, Create(Arr_Inner));
						end;
					end loop;
					Result.Set_Field("result", Create(Arr));
				end;
		end case;

		for E of Param_Values loop
			Free(E);
		end loop;

		return Result;
	end;
end;
