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

------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Db; use Db;
with Db.Stmts; use Db.Stmts;
with Db.Executors; use Db.Executors;
with AWS.Status; use AWS.Status;
with AWS.Session; use AWS.Session;
with AWS.Parameters; use AWS.Parameters;
with Execute_JSON; -- use Ajax_Execute;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;
with Util; use Util;
with WebSocket; use WebSocket;
with Image_Handling; use Image_Handling;

package body Ajax is

	function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data is
		Orig : constant String := URI (Request);
		Method : constant String := Orig(Orig'First + 6 .. Orig'Last);
		Statement_To_Execute : Ajax_Statement;
		P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
		S : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		if not aws.session.exist(S, "user_id") then
			log ("[/ajax/" & method & "]", "Not logged in. URL: " & URL(Request) & ", Headers: " & Headers_To_String(Header(Request)), emergency);
			return Json_To_Response(Error_Json ("You are not logged in."));
		end if;

		begin
			Statement_To_Execute := Ajax_Statement'Value(Method);
		exception
			when Error : others =>
				log_exception (Error, "[/ajax/" & method & "] Supplied parameters: " & Parameters_To_String(P));
				return Json_To_Response(Error_Json ("Methode nicht vorhanden."));
		end;

		log ("", "[/ajax/" & method & "] Supplied parameters: " & Parameters_To_String(P), debug);

		declare
			Params : Execute_JSON.Params_T(1 .. Integer(Statement_Parameters(Statement_To_Execute).Parameters));
		begin
			-- Param_Values := new Param_Values_T(1 .. Size_T(Statement_Parameters(Statement_To_Execute).Parameters));

			Params(1) := To_Unbounded_String(String'(AWS.Session.Get(S, "user_id")));
			Params(2) := To_Unbounded_String(String'(AWS.Session.Get (S, "group_id")));
			Params(3) := To_Unbounded_String(String'(AWS.Session.Get (S, "character_id")));

			for I in 4 .. Params'Last loop
				declare
					Key : String := "arg" & Natural_To_String(I - 3);
				begin
					Params(I) := To_Unbounded_String(AWS.Parameters.Get(P, Key));
					log ("[/ajax/" & method & "]", "argument " & Key & ": " & AWS.Parameters.Get(P, Key), debug);
				end;
			end loop;

			declare
				Reraise_No_Lines_Found : Boolean := Statement_Parameters(Statement_To_Execute).Return_Type = DB.Stmts.Single;
				Result : JSON_Array := Execute_JSON.Execute(Statement_To_Execute, Params, Reraise_No_Lines_Found);
			begin
				if Statement_To_Execute in Notifiable_Statement then
					log("[ajax]", "Notifiable Statement.", Debug);
					Notify(Statement_To_Execute, Params, Result);
					if Statement_To_Execute in user_delete | group_delete | character_delete | gallery_delete then
						for I in 1 .. Length(Result) loop
							Image_Handling.Delete_Image(Get(Get(Get(Get(Result, I)), 1)));
						end loop;
					elsif Statement_To_Execute = image_delete then
						Image_Handling.Delete_Image(Get(Get(Result, 1)));
					end if;
				end if;

				if Statement_To_Execute = image_create_with_permission then
					declare
						Image_Url : String := To_String(Params(8));
					begin
						Process_Image(Image_Url);
					exception
						when Error : others =>
							log_exception (Error, "[/ajax/" & method & "] Probably Image Url manipulated: " & Parameters_To_String(P) & ", deleting Image.");
							Params(4) := To_Unbounded_String(String'(Get(Get(Result, 1))));
							Result := Execute_JSON.Execute(image_delete, Params(1..4));
							return Json_To_Response(Error_Json ("Probably you did bad stuff with the image name?"));
					end;
				end if;

				return Json_To_Response(Success_Json(Create(Result)));
			end;

		exception
			when No_Lines_Found =>
				return Json_To_Response(Success_Json(Create));
			when Error : others =>
				log_exception (Error, "[/ajax/" & method & "]");
				return Json_To_Response(Error_Json (Exception_To_String(Error)));
		end;
	end HW_CB;
end Ajax;
