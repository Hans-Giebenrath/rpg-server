-- Copyright (C) 2015 Hans Giebenrath
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>

with Db; use Db;
with Db.Stmts; use Db.Stmts;
with Db.Executors; use Db.Executors;
with AWS.Status; use AWS.Status;
with AWS.Session; use AWS.Session;
with AWS.Parameters; use AWS.Parameters;
with Gnatcoll.Json; use Gnatcoll.Json;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation; 
with Util; use Util;
with Execute_JSON;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
use Ada.Strings.UTF_Encoding;

package body role is

	function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data is

		Orig : String := URI (Request);
		Method : constant String := Orig(Orig'First + 6 .. Orig'Last);
		Statement_To_Execute : Role_Statement;
		P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
		Result : JSON_Value := Create_Object;
		S  : constant AWS.Session.ID := AWS.Status.Session (Request);
		Param_Values : Param_Values_T_Access;
	begin
		begin
			Statement_To_Execute := Role_Statement'Value(Method);
		exception
			when Error : others =>
				-- do not return "Method", as it may contains XSS
				return Json_To_Response(Error_Json("Methode nicht vorhanden."));
		end;

		case Statement_To_Execute is
			when register =>
				Param_Values := new Param_Values_T (1 .. 3);
				Param_Values(1) := To_Chars_Ptr(UTF_8_String'(Encode(AWS.Parameters.Get(P, "name"))));
				Param_Values(2) := To_Chars_Ptr(UTF_8_String'(Encode(AWS.Parameters.Get(P, "password"))));
				Param_Values(3) := To_Chars_Ptr(UTF_8_String'(Encode(AWS.Parameters.Get(P, "email"))));

				log ("[/role/register]", "name: " & AWS.Parameters.Get(P, "name") & ", pw: (just kidding), email: " & AWS.Parameters.Get(P, "email"), debug);
				begin
					Execute(Param_Values, Stmts.Register);
					-- as a registered user is not allowed to do something inside of the system, delete everything.
					AWS.Session.Remove(S, "user_id");
					AWS.Session.Remove(S, "group_id");
					AWS.Session.Remove(S, "character_id");
					Result := Success_Json(Result);
				exception
					when Error : others =>
						Result := Error_Json("Registrierung war nicht erfolgreich. (Nutzername schon vorhanden.)");
						log_exception(Error, "[/role/register]");
				end;
			when login =>
				Param_Values := new Param_Values_T (1 .. 2);
				Param_Values(1) := To_Chars_Ptr(AWS.Parameters.Get(P, "name"));
				Param_Values(2) := To_Chars_Ptr(AWS.Parameters.Get(P, "password"));
				log ("[/role/login]", "name: " & AWS.Parameters.Get(P, "name"), debug);

				declare
					RS : Executors.Single(Param_Values, Stmts.Login);
					Row : Row_Data_Access;
				begin
					Row := Execute(RS);
					AWS.Session.Set(S, "user_id", To_String(Row(Row'First)));
					AWS.Session.Remove(S, "group_id");
					AWS.Session.Remove(S, "character_id");
				exception
					when Error : others =>
						Result := Error_Json("Login was not succesfull.");
						log_exception(Error, "[/role/login]");
				end;

				declare
					Result_Own : JSON_Value := Create_Object;
					Result_Own_Params : Execute_JSON.Params_T(1 .. 3);
				begin
					Result_Own_Params(1) := To_Unbounded_String(String'(AWS.Session.Get(S, "user_id")));
					Result_Own_Params(2) := To_Unbounded_String("");
					Result_Own_Params(3) := To_Unbounded_String("");
					Result_Own.Set_Field("chars", Execute_JSON.execute(own_characters_list, Result_Own_Params, False));
					Result_Own.Set_Field("gms", Execute_JSON.execute(own_gms_list, Result_Own_Params, False));
					Result.Set_Field("own", Result_Own);
					Result.Set_Field("user_id", Create(String'(AWS.Session.Get(S, "user_id"))));
					-- user_list is not allowed to return an empty result set, as the user
					-- currently logging in ... is a user.
					Result.Set_Field("user", Execute_JSON.execute(user_list, Result_Own_Params));
					Result := Success_Json(Result);
				exception
					when Error : others =>
						Result := Error_Json("Retrieval of information from the db failed.");
						log_exception(Error, "[/role/login]");
				end;
			when check_role_gm =>
				-- use this also for switching role
				log ("[/role/check_role_gm]", "group_id: " & AWS.Parameters.Get(P, "group_id"), debug);
				declare
					Group_Id_P : String := AWS.Parameters.Get(P, "group_id");
					Result_Group : JSON_Value := Create_Object;
					Result_Group_Params : Execute_JSON.Params_T(1 .. 3);
				begin
					Param_Values := new Param_Values_T(1 .. 2);
					Param_Values(1) := To_Chars_Ptr(AWS.Session.Get(S, "user_id"));
					Param_Values(2) := To_Chars_Ptr(Group_Id_P);

					Execute (Param_Values, Stmts.check_role_gm);
					-- no exception => success
					AWS.Session.Set(S, "group_id", Group_Id_P);
					AWS.Session.Remove(S, "character_id");
					Result_Group_Params(1) := To_Unbounded_String(String'(AWS.Session.Get(S, "user_id")));
					Result_Group_Params(2) := To_Unbounded_String(Group_Id_P);
					Result_Group_Params(3) := To_Unbounded_String("");
					Result_Group.Set_Field("chars", Execute_JSON.execute(group_characters_list, Result_Group_Params, False));
					Result_Group.Set_Field("gms", Execute_JSON.execute(group_gms_list, Result_Group_Params, False));
					Result_Group.Set_Field("info", Execute_JSON.execute(group_view, Result_Group_Params, False));
					Result_Group.Set_Field("wiki", Execute_JSON.execute(wiki_list, Result_Group_Params, False));
					Result_Group.Set_Field("gallery", Execute_JSON.execute(gallery_list, Result_Group_Params, False));
					Result_Group.Set_Field("history", Execute_JSON.execute(history_list, Result_Group_Params, False));
					Result_Group.Set_Field("tag", Execute_JSON.execute(tag_list, Result_Group_Params, False));
					Result_Group.Set_Field("appointment", Execute_JSON.execute(appointment_list, Result_Group_Params, False));
					Result.Set_Field("group", Result_Group);
					Result := Success_Json(Result);
				exception
					when Error : others =>
						Result := Error_Json("You are not the group's gamemaster.");
						log_exception(Error, "[/role/check_role_gm]");
				end;
			when check_role_character =>
				-- use this also for switching role
				log ("[/role/check_role_character]", "group_id: " & AWS.Parameters.Get(P, "group_id") & ", character_id: " & AWS.Parameters.Get(P, "character_id"), debug);
				declare
					Group_Id_P : String := AWS.Parameters.Get(P, "group_id");
					Character_Id_P : String := AWS.Parameters.Get(P, "character_id");
					Result_Group : JSON_Value := Create_Object;
					Result_Group_Params : Execute_JSON.Params_T(1 .. 3);
				begin
					Param_Values := new Param_Values_T(1 .. 3);
					Param_Values(1) := To_Chars_Ptr(AWS.Session.Get(S, "user_id"));
					Param_Values(2) := To_Chars_Ptr(Group_Id_P);
					Param_Values(3) := To_Chars_Ptr(Character_Id_P);

					Execute (Param_Values, Stmts.check_role_character);
					-- no exception => success
					AWS.Session.Set(S, "group_id", Group_Id_P);
					AWS.Session.Set(S, "character_id", Character_Id_P);
					log("[check_role_character]" , "Stored character_id: " & Character_Id_P, debug);
					Result_Group_Params(1) := To_Unbounded_String(String'(AWS.Session.Get(S, "user_id")));
					Result_Group_Params(2) := To_Unbounded_String(Group_Id_P);
					Result_Group_Params(3) := To_Unbounded_String(Character_Id_P);
					Result_Group.Set_Field("chars", Execute_JSON.execute(group_characters_list, Result_Group_Params, False));
					Result_Group.Set_Field("gms", Execute_JSON.execute(group_gms_list, Result_Group_Params, False));
					Result_Group.Set_Field("info", Execute_JSON.execute(group_view, Result_Group_Params, False));
					Result_Group.Set_Field("wiki", Execute_JSON.execute(wiki_list, Result_Group_Params, False));
					Result_Group.Set_Field("gallery", Execute_JSON.execute(gallery_list, Result_Group_Params, False));
					Result_Group.Set_Field("history", Execute_JSON.execute(history_list, Result_Group_Params, False));
					Result_Group.Set_Field("tag", Execute_JSON.execute(tag_list, Result_Group_Params, False));
					Result_Group.Set_Field("appointment", Execute_JSON.execute(appointment_list, Result_Group_Params, False));
					Result.Set_Field("group", Result_Group);
					Result := Success_Json(Result);
				exception
					when Error : others =>
						Result := Error_Json("You do not own the character.");
						log_exception(Error, "[/role/check_role_character]");
				end;
		end case;

		for E of Param_Values.all loop
			Free(E);
		end loop;

		Free (Param_Values);
		return Json_To_Response(Result);
	end HW_CB;
end role;
