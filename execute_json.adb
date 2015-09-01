with Ada.Strings.Unbounded;
with Util; use Util;
with Gnatcoll.Json; use Gnatcoll.Json;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Deallocation; 
with Db; use Db;
with Db.Stmts; use Db.Stmts;
with Db.Executors; use Db.Executors;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
use Ada.Strings.UTF_Encoding;

package body Execute_JSON is
	function execute (Statement_To_Execute : Ajax_Statement; Params : Params_T; Reraise_No_Lines_Found : Boolean := True) return JSON_Array is

		Result : JSON_Array := Empty_Array;
		-- type Param_Values_T_Access is access Param_Values_T;
		-- procedure Free is new Ada.Unchecked_Deallocation(Param_Values_T, Param_Values_T_Access);
		-- Param_Values : Param_Values_T_Access := new Param_Values_T(1 .. Size_T(Params'Last - Params'First + 1));
		Param_Values : Param_Values_T_Access := new Param_Values_T(Size_T(Params'First) .. Size_T(Params'Last));
	begin
		-- declare
			-- function To_Chars_Ptr(S : String) return chars_ptr is
				-- (if S = "" then null_ptr else New_String(UTF_8_String'(Encode(S))))
				-- with Inline;
		begin
			for I in Params'Range loop
				Param_Values(Size_T(I)) := To_Chars_Ptr(To_String(Params(I)));
			end loop;
		end;

		case Statement_Parameters(Statement_To_Execute).Return_Type is
			when Stmts.Void =>
				Execute(Param_Values, Statement_To_Execute);
			when Stmts.Single =>
				declare
					RS : Executors.Single(Param_Values, Statement_To_Execute);
					Row : Row_Data_Access;
					-- Arr : Json_Array := Empty_Array;
				begin
					Row := Execute (RS);
					for I in Row.all'Range loop
						Append(Result, Create(To_String(Row(I))));
					end loop;
					-- Result := Create(Arr);
				end;
			when Stmts.Table =>
				declare
					RS : Executors.Table (Param_Values, Statement_To_Execute);
					-- Arr : Json_Array := Empty_Array;
				begin
					for Row of RS loop
						declare
							Arr_Inner : Json_Array := Empty_Array;
						begin
							for I in Row.all'Range loop
								Append(Arr_Inner, Create(To_String(Row(I))));
							end loop;
							Append(Result, Create(Arr_Inner));
						end;
					end loop;
					-- Result := Create(Arr);
				end;
		end case;

		for E of Param_Values.all loop
			Free(E);
		end loop;
		Free(Param_Values);

		return Result;
	exception
		when No_Lines_Found =>
			if Reraise_No_Lines_Found then
				raise;
			else
				return Empty_Array;
			end if;
	end;
end;
