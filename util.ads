with AWS.Response; use AWS.Response;
with AWS.Parameters;
with Ada.Exceptions;
with Gnatcoll.Json; use Gnatcoll.Json;
with Execute_Json; use Execute_Json;
with AWS.Headers;

package Util is
	type Log_Level_Type is (none, emergency, alert, critical, error, warning, notice, info, debug);
	Log_Level : Log_Level_Type := debug;

	function Exception_To_String (E : Ada.Exceptions.Exception_Occurrence; Prefix : String := "") return String;
	procedure log (Prefix : String; Message : String; Level : Log_Level_Type);
	procedure log_exception (E : Ada.Exceptions.Exception_Occurrence; Prefix : String; Level : Log_Level_Type := Error);
	function Error_Json (Msg : String) return Json_Value
		with Inline;
	function Success_Json (Payload : Json_Value := JSON_Null) return Json_Value
		with Inline;

	function Json_To_Response (Payload : Json_Value) return AWS.Response.Data
		with Inline;

	function Parameters_To_String (P : AWS.Parameters.List) return String;
	function Parameters_To_String (P : Params_T) return String;

	function Natural_To_String (N : Natural) return String
		with Inline;
	function generate_random_string (n : in Positive) return String;

	function Headers_To_String (H : AWS.Headers.List) return String;

	-- function generate_random_string (n : in Positive) return String;

	-- function generate_random_websocket_id return Websocket_Id_T;

	subtype Random_Filename_T is String(1 .. 16);
	function generate_random_string return Random_Filename_T is
		(generate_random_string(Random_Filename_T'Length));
	
	Dir_Separator : constant Character;                                               
	pragma Import (C, Dir_Separator, "__gnat_dir_separator");
end Util;
