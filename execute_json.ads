with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;
with Db.Stmts; use Db.Stmts;

package Execute_JSON is
	type Params_T is array (Integer range <>) of Unbounded_String;
	function execute (Statement_To_Execute : Ajax_Statement; Params : Params_T; Reraise_No_Lines_Found : Boolean := True) return JSON_Array;
end;
