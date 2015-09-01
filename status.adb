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

with AWS.Status; use AWS.Status;
with AWS.Session; use AWS.Session;
with AWS.Parameters; use AWS.Parameters;
with Gnatcoll.Json; use Gnatcoll.Json;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation; 
with Util; use Util;
with Execute_JSON;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
with Db.Stmts; use Db.Stmts;
use Ada.Strings.UTF_Encoding;

package body status is
	type Desire_T is (Ids);

	function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data is
		Orig : String := URI (Request);
		Method : constant String := Orig(Orig'First + 8 .. Orig'Last);
		Desire : Desire_T;
		Result : JSON_Value := Create_Object;
		S  : constant AWS.Session.ID := AWS.Status.Session (Request);
		User_Id : String := AWS.Session.Get(S, "user_id");
		Group_Id : String := AWS.Session.Get(S, "group_id");
		Character_Id : String := AWS.Session.Get(S, "character_id");
		Params : Execute_JSON.Params_T(1 .. 3) := (To_Unbounded_String(User_Id), To_Unbounded_String(Group_Id), To_Unbounded_String(Character_Id));
	begin
		begin
			Desire := Desire_T'Value(Method);
		exception
			when Error : others =>
				-- do not return "Method", as it may contains XSS
				return Json_To_Response(Error_Json("Methode nicht vorhanden."));
		end;

		case Desire is
			when Ids =>
				Result.Set_Field("user_id", Create(User_Id));
				Result.Set_Field("group_id", Create(Group_Id));
				Result.Set_Field("character_id", Create(Character_Id));

				if User_Id /= "" then
					declare
						Result_Own : JSON_Value := Create_Object;
					begin
						Result_Own.Set_Field("chars", Execute_JSON.execute(own_characters_list, Params, False));
						Result_Own.Set_Field("gms", Execute_JSON.execute(own_gms_list, Params, False));
						Result.Set_Field("own", Result_Own);
						-- user_list is not allowed to return an empty result set, as the user
						-- currently logging in ... is a user.
						Result.Set_Field("user", Execute_JSON.execute(user_list, Params));
					exception
						when Error : others =>
							Result := Error_Json("Retrieval of information from the db failed.");
							log_exception(Error, "[/role/login]");
					end;
				end if;

				if Group_Id /= "" then
					declare
						Result_Group : JSON_Value := Create_Object;
					begin
						Result_Group.Set_Field("chars", Execute_JSON.execute(group_characters_list, Params, False));
						Result_Group.Set_Field("gms", Execute_JSON.execute(group_gms_list, Params, False));
						Result_Group.Set_Field("info", Execute_JSON.execute(group_view, Params, False));
						Result_Group.Set_Field("wiki", Execute_JSON.execute(wiki_list, Params, False));
						Result_Group.Set_Field("gallery", Execute_JSON.execute(gallery_list, Params, False));
						Result_Group.Set_Field("history", Execute_JSON.execute(history_list, Params, False));
						Result_Group.Set_Field("tag", Execute_JSON.execute(tag_list, Params, False));
						Result_Group.Set_Field("appointment", Execute_JSON.execute(appointment_list, Params, False));
						Result.Set_Field("group", Result_Group);
						Result := Success_Json(Result);
					exception
						when Error : others =>
							Result := Error_Json("You do not own the character.");
							log_exception(Error, "[/role/check_role_character]");
					end;
				end if;

				Result := Success_Json(Result);
		end case;

		return Json_To_Response(Result);
	end HW_CB;
end status;
