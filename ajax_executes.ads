with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;
with Db.Stmts; use Db.Stmts;

package Ajax_Executes is
	type Params_T is array (Integer range <>) of Unbounded_String;
	function execute (Statement_To_Execute : Ajax_Statement; Params : Params_T) return JSON_Value;
end;
