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

with db; use db;
with db.stmts; use db.stmts;
with Execute_JSON; use Execute_JSON;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;
with AWS.Net.WebSocket; use AWS.Net.WebSocket;
with AWS.Status; use AWS.Status;
with AWS.Net; use AWS.Net;
with AWS.Session; use AWS.Session;
use AWS;
package websocket is
	type MySocket is new Net.WebSocket.Object with private;

	function Create (Socket  : Socket_Access; Request : AWS.Status.Data) return Net.WebSocket.Object'Class;
	procedure Notify (Stmt : Notifiable_Statement; Params : Params_T; Result : JSON_Array);

private
	subtype Character_Id_T is Natural;
	subtype Group_Id_T is Natural;
	subtype User_Id_T is Natural;
	No_Character_Id : Character_Id_T := 0;
	No_Group_Id : Group_Id_T := 0;
	No_User_Id : User_Id_T := 0;

	subtype Websocket_Id_T is String(1 .. 12);

	type MySocket is new Net.WebSocket.Object with 
		record
			Session : AWS.Session.Id;
			Id : Websocket_Id_T;
			User_Id_S : Unbounded_String := Null_Unbounded_String;
			Group_Id_S : Unbounded_String := Null_Unbounded_String;
			Character_Id_S : Unbounded_String := Null_Unbounded_String;
			User_Id : User_Id_T := No_User_Id;
			Group_Id : Group_Id_T := No_Group_Id;
			Character_Id : Character_Id_T := No_Character_Id;
		end record;
	-- overriding
	-- function Create (Socket  : Socket_Access; Request : AWS.Status.Data) return MySocket'Class;

	overriding
	procedure On_Close (Socket : in out MySocket; Message : String);

	overriding
	procedure On_Open (Socket : in out MySocket; Message : String);

	overriding
	procedure On_Message (Socket : in out MySocket; Message : String);


end websocket;
