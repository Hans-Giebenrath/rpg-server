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

-- TODO cleanup websockets, if user or group gets deleted
-- Let's see, if this is really necessary in reality ... As they close itself.
with AWS.Net.WebSocket; use AWS.Net.WebSocket;
with Interfaces.C; use Interfaces.C;
with db; use db;
with AWS.Status; use AWS.Status;
with AWS.Headers; use AWS.Headers;
with AWS.Messages; use AWS.Messages;
with AWS.Session; use AWS.Session;
with Util; use Util;
with Ada.Exceptions;
with Gnatcoll.Json; use Gnatcoll.Json;
with Execute_JSON;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body websocket is

	type Queue_Data_Kind is (Edit_Delete, Create);
	package Params_T_Holders is new Ada.Containers.Indefinite_Holders(Params_T);
	use Params_T_Holders;
	type Queue_Data (Kind : Queue_Data_Kind) is
		record
			Stmt : Notifiable_Statement;
			case Kind is
				when Edit_Delete =>
					-- Params : Params_T_Holders.Holder;
					Params : Params_T_Holders.Holder;
				when Create =>
					Result : JSON_Array;
					User_Id : User_Id_T;
					Group_Id : Group_Id_T;
			end case;
		end record;
	package Queue_Data_Holders is new Ada.Containers.Indefinite_Holders(Queue_Data);
	use Queue_Data_Holders;

	package Notification_Queue_Interface is
		new Synchronized_Queue_Interfaces(Element_Type => Queue_Data_Holders.Holder);

	package Notification_Queues is
		new Unbounded_Synchronized_Queues(Queue_Interfaces => Notification_Queue_Interface);
	use Notification_Queues;

	function Separator_Position (Message : String) return Integer is
	begin
		for I in Message'Range loop
			if Message(I) = ':' then
				return I;
			end if;
		end loop;
		
		raise Constraint_Error with "Malformed Message";
	end;

	-- protected 
	type MySocket_Access is access all MySocket;

	function To_String(S : MySocket) return String is
		("Id: " & S.Id & ", FD:" & Integer'Image(Get_FD(S)) & ", user_id: " & To_String(S.User_Id_S) & ", group_id: " & To_String(S.group_id_S) & ", character_id: " & To_String(S.Character_Id_S));

	function "=" (Left, Right : MySocket_Access) return Boolean is
		(Left.all.Id = Right.all.Id);

	package Open_Sockets_Vectors is new Vectors(Index_Type => Positive, Element_Type => MySocket_Access);
	use Open_Sockets_Vectors;
	-- type Open_Sockets_Vector_Access is access Open_Sockets_Vectors.Vector;
	-- procedure Free is new Ada.Unchecked_Deallocation(Open_Sockets_Vectors.Vector, Open_Sockets_Vector_Access);

	package MySocket_Maps is new Ada.Containers.Ordered_Maps (Key_Type => FD_Type, Element_Type => Open_Sockets_Vectors.Vector);
	
	package User_Maps is new Ada.Containers.Ordered_Maps (Key_Type => User_Id_T, Element_Type => Open_Sockets_Vectors.Vector);
	use User_Maps;

	function "=" (Left, Right:User_Maps.Map) return Boolean is
		(false);
	-- as the gm has no character_id, use the user_maps to in group_maps
	package Group_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Group_Id_t, Element_Type => User_Maps.Map);
	use Group_Maps;

	package Closed_Sockets_Vectors is new Vectors(Index_Type => Positive, Element_Type => MySocket_Access);
	
	procedure Send_Json (S : in out MySocket; Stmt : String; Content : JSON_Value) is
	begin
		log("[websocket.listener.send_json]", "Sending to " & To_String(S), Debug);
		Send(Socket => S, Message => String'(Stmt & ":" & Write(Content)));
		-- AWS.Net.Websocket.Send(S, Stmt & ":" & Write(Content));
	exception
		when Error : Socket_Error =>
			log_exception(Error, "[websocket.listener.send_Json]");
			raise;
	end;

	protected Registry is
		-- For the Query_Reader
		-- NOTE on a new version of AWS, they can be functions (as S does not have to be "in out")
		procedure Query_Group(Group_Id : Natural; Process : not null access procedure (S : in out MySocket));
		procedure Query_User(User_Id : Natural; Process : not null access procedure (S : in out MySocket));
		procedure Query_All_Users(Process : not null access procedure (S : in out MySocket));
		-- For the Sockets
		procedure Enter_User (S : in out MySocket);
		procedure Enter_Group (S : in out MySocket);
		procedure Switch_Group (S : in out MySocket; Old_Group_Id : Group_Id_T);
		-- this will just delete the socket out of every map.
		procedure Delete (S : in out MySocket);
		-- this will propagate a message to all sockets of that user, that they
		-- should log out.
		procedure Logout (S : in out MySocket);
		-- For debugging
		procedure Log_Maps;
	private
		Group_Map : Group_Maps.Map;
		User_Map : User_Maps.Map;
		Closed_Sockets : Closed_Sockets_Vectors.Vector;
	end Registry;


	-- NOTE Could be split up into a lot of queues, if they get big and there are
	-- several listeners.
	Notification_Queue : Notification_Queues.Queue;

	function Create (Socket : Socket_Access; Request : AWS.Status.Data) return Net.WebSocket.Object'Class is
	begin
		return
			MySocket'(AWS.Net.WebSocket.Object
				(AWS.Net.WebSocket.Create (Socket, Request)) with
				Session => No_Session,
				Id => generate_random_string(Websocket_Id_T'Last),
				User_Id_S => Null_Unbounded_String,
				Group_Id_S => Null_Unbounded_String,
				character_Id_S => Null_Unbounded_String,
				User_Id => No_User_Id,
				Group_Id => No_Group_Id,
				Character_Id => No_Character_Id
				);
	end Create;

	procedure On_Open (Socket : in out MySocket; Message : String) is 
	begin
		log("[websocket.on_open]", "Received Message: " & Message, Debug);
	exception
		when Error : others =>
			log_exception(Error, "[websocket.on_open]");
	end;

	type Incoming_Messages is (Enter_Group, Switch_Group, Enter_User, Logout);
	procedure On_Message (Socket : in out MySocket; Message : String) is 
		Separator : Integer := Separator_Position(Message);
	begin
		log("[websocket.on_message]", Message & ", Separator_Position:" & Integer'Image(Separator), Debug);
		case Incoming_Messages'Value(Message(Message'First .. Separator - 1)) is
			when Enter_User =>
				Socket.Session := AWS.Session.Value(Message(Separator + 1 .. Message'Last));
				declare
					User_Id : String := AWS.Session.Get(Socket.Session, "user_id");
				begin
					log("[websocket.on_message.enter_user]", "User_Id(Session): " & User_Id, Debug);
					if User_ID = "" then
						Socket.Session := No_Session;
						Socket.User_Id_S := Null_Unbounded_String;
						Socket.User_ID := No_User_Id;
						raise Constraint_Error with "The Session_ID is not connected to a user_id.";
					end if;
					Socket.User_ID_S := To_Unbounded_String(User_Id);
					Socket.User_Id := User_Id_T'Value(User_Id);
				end;
				Registry.Enter_User(Socket);
			when Enter_Group =>
				declare
					Group_Id : String := AWS.Session.Get(Socket.Session, "group_id");
					Character_Id : String := AWS.Session.Get(Socket.Session, "character_id");
				begin
					log("[websocket.on_message.enter_group]", "Group_Id(Session): " & Group_Id & ", Character_Id(Session): " & Character_Id, Debug);
					if Group_Id = "" then
						Socket.Group_Id_S := Null_Unbounded_String;
						Socket.Character_Id_S := Null_Unbounded_String;
						Socket.Group_Id := No_Group_Id;
						Socket.Character_Id := No_Character_Id;
						raise Constraint_Error with "The Session_ID is not connected to a group_id.";
					end if;

					Socket.Group_Id_S := To_Unbounded_String(Group_Id);
					Socket.Group_Id := Group_Id_T'Value(Group_Id);

					if Character_Id = "" then
						Socket.Character_Id_S := Null_Unbounded_String;
						Socket.Character_Id := No_Character_Id;
					else
						Socket.Character_Id_S := To_Unbounded_String(Character_Id);
						Socket.Character_Id := Character_Id_T'Value(Character_Id);
					end if;
				end;
				Registry.Enter_Group(Socket);
			when Switch_Group =>
				declare
					Group_Id : String := AWS.Session.Get(Socket.Session, "group_id");
					Character_Id : String := AWS.Session.Get(Socket.Session, "character_id");
				begin
					log("[websocket.on_message.switch_group]", "Group_Id(Session): " & Group_Id & ", Character_Id(Session): " & Character_Id, Debug);
					if Group_Id = "" then
						Socket.Group_Id_S := Null_Unbounded_String;
						Socket.Character_Id_S := Null_Unbounded_String;
						Socket.Group_Id := No_Group_Id;
						Socket.Character_Id := No_Character_Id;
						raise Constraint_Error with "The Session_ID is not connected to a group_id.";
					end if;

					Socket.Group_Id_S := To_Unbounded_String(Group_Id);
					Socket.Group_Id := Group_Id_T'Value(Group_Id);

					if Character_Id = "" then
						Socket.Character_Id_S := Null_Unbounded_String;
						Socket.Character_Id := No_Character_Id;
					else
						Socket.Character_Id_S := To_Unbounded_String(Character_Id);
						Socket.Character_Id := Character_Id_T'Value(Character_Id);
					end if;
				end;
				Registry.Switch_Group(Socket, Group_Id_T'Value(Message(Separator + 1 .. Message'Last)));
			when Logout => 
				AWS.Session.Delete(Socket.Session);
				Registry.Logout(Socket);
		end case;
		log("[websocket.on_message]" , "Successfully finished. Set: " & To_String(Socket), debug);
	exception
		when Error : others =>
			log_exception(Error, "[websocket.on_message]");
			raise;
	end;

	procedure On_Close (Socket : in out MySocket; Message : String) is 
	begin
		log("[websocket.on_close]", Message, Debug);
		log("[websocket.on_close]", "Closing " & To_String(Socket), Debug);
		Registry.Delete(Socket);
		log("[websocket.on_close]", "Successfully deleted session.", Debug);
	end;

	Task Listener;
	Task Body Listener is
		Data : Queue_Data_Holders.Holder;

		procedure Send_Json (S : in out MySocket; Stmt : Statement; Content : JSON_Value) is
		begin
			Send_Json(S, String'(Statement'Image(Stmt)), Content);
		end;

		procedure Process_Edit (Stmt : Edit_Statement; Params : Params_T) is -- {{{
			B : Integer := Params'First;
			User_Id : User_Id_T := User_Id_T'Value(To_String(Params(B)));
			Group_Id : Group_Id_T := Group_Id_T'Value(To_String(Params(B + 1)));
		begin
			log("[listener.process_edit]", "Stmt: " & Statement'Image(Stmt), Debug);
			case Stmt is
				when user_edit =>
					declare
						Content : JSON_Value;
						procedure P (S : in out MySocket) is
						begin
							Send_Json(S, User_View, Content);
						end;
					begin
						Content := Create(Create(To_String(Params(B))) & Create(To_String(Params(B + 4))));
						Registry.Query_All_Users(P'Access);
					end;
				when character_owner_edit =>
					declare
						Content : JSON_Value := Create_Object;

						Transferred_Character : String := To_String(Params(B + 3));

						From_User : String := To_String(Params(B + 5));
						From_User_I : User_Id_T := User_Id_T'Value(From_User);

						To_User : String := To_String(Params(B + 4));
						To_User_I : User_Id_T := User_Id_T'Value(To_User);

						From_Send : Boolean := False;
						To_Send : Boolean := False;

						procedure P (S : in out MySocket) is
						begin
							log("[listener.process_edit.character_owner_edit.p]", "P entered.", Debug);

							if S.User_Id = To_User_I then
								To_Send := True;
							end if;

							if S.User_Id = From_User_I then
								From_Send := True;
							end if;

							Send_Json(S, character_owner_edit, Content);
						end;

						procedure Q (S : in out MySocket) is
							Params : Execute_JSON.Params_T(1 .. 3) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
						begin
							log("[listener.process_edit.character_owner_edit.q]", "Q entered.", Debug);
							Send_Json(S, own_characters_list, Create(Execute_JSON.Execute(own_characters_list, Params)));
						end;
					begin
						log("[listener.process_edit.character_owner_edit]", "entered.", Debug);
						if From_User_I /= To_User_I then
							Content.Set_Field("character_id", Create(Transferred_Character));
							Content.Set_Field("user_id", Create(To_User));
							Registry.Query_Group(Group_Id, P'Access);

							if not To_Send then Registry.Query_User(To_User_I, P'Access); end if;
							Registry.Query_User(To_User_I, Q'Access);

							if not From_Send then Registry.Query_User(From_User_I, P'Access); end if;

							pragma assert(From_Send, "There was no message send to ""from""");
							pragma assert(To_Send, "There was no message send to ""to""");
						else
							log("[listener.process_edit.character_owner_edit]", "From and To are equal.", debug);
						end if;
					end;
				when others =>
					case Stmt is
						when wiki_edit =>
							declare
								procedure P (S : in out MySocket) is
									Return_Result : JSON_Array;
									New_Params : Execute_JSON.Params_T(1 .. 3);
								begin
									New_Params := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
									log ("[process_edit.others.wiki_edit]", "Calling Tag_List", Debug);
									Return_Result := Execute_JSON.Execute(Tag_List, New_Params);
									Send_Json(S, Tag_List, Create(Return_Result));
								exception
									when No_Lines_Found =>
										log("[process_edit.others.wiki_edit]", "No Tags found.", Debug);
								end;
							begin
								Registry.Query_Group(Group_Id, P'Access);
							end;
						when others =>
							null;
					end case;

					declare
						Answer : Statement := (
							case Stmt is
								when character_edit => character_view,
								when character_set_image => character_view,
								when gallery_edit => gallery_view,
								when appointment_edit => appointment_view,
								when image_edit => image_view,
								when image_order_edit => image_order_list,
								when gallery_set_image => gallery_view,
								when history_edit => history_view,
								when history_order_edit => history_order_list,
								when wiki_edit => wiki_view,
								when permission_update => permission_list,
								when others => raise Constraint_Error with "Missing enum in case."
						);
						procedure P (S : in out MySocket) is
							Return_Result : JSON_Array;
							New_Params : Execute_JSON.Params_T(1 .. Integer(Statement_Parameters(Answer).Parameters));
						begin
							New_Params(1 .. 3) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
							if New_Params'Length = 4 then
								New_Params(4) := Params(4);
							end if;
							log ("[process_edit.others.p]", "Stmt: " & Statement'Image(Answer) & ", Supplied Parameters: " & Parameters_To_String(Params), Debug);
							Return_Result := Execute_JSON.Execute(Answer, New_Params);
							Send_Json(S, Answer, Create(Return_Result));

							if Stmt in gallery_edit | image_edit | wiki_edit | history_edit then
								begin
									New_Params(4) := Params(Params'Last);
									Send_Json(S, permission_list, Create(Execute_JSON.Execute(permission_list, New_Params)));
								exception
									when No_Lines_Found =>
										null;
								end;
							end if;

						exception
							when No_Lines_Found =>
								log("[process_edit.others.p]", "Nothing found.", Debug);
							when Error : others =>
								log_exception(Error, "[process_edit.others.p]");
						end;
					begin
						Registry.Query_Group(Group_Id, P'Access);
					end;
			end case;
		end; -- }}}

		procedure Process_Delete (Stmt : Delete_Statement; Params : Params_T) is -- {{{
			B : constant Integer := Params'First;
			User_Id : User_Id_T := User_Id_T'Value(To_String(Params(B)));
			Group_Id : Group_Id_T := Group_Id_T'Value(To_String(Params(B + 1)));
			Content : JSON_Value;
			procedure P (S : in out MySocket) is
			begin
				Send_Json(S, Stmt, Content);
			end;
		begin
			-- todo a lot of people should be notified ...
			case stmt is
				when group_delete =>
					Content := Create(Group_Id);
					-- another solution would be to first fetch (before deletion in ajax) all group members,
					-- and then send those users the stuff.
					Registry.Query_All_Users(P'Access);
				when character_delete =>
					Content := Create(To_String(Params(B + 3)));
					Registry.Query_All_Users(P'Access);
				when user_delete =>
					Content := Create(To_String(Params(B)));
					Registry.Query_All_Users(P'Access);
				when gm_delete =>
					Content := Create(Create(To_String(Params(B + 3))) & Create(Group_Id));
					Registry.Query_All_Users(P'Access);
				when appointment_delete =>
					Content := Json_Null;
					Registry.Query_Group(Group_Id, P'Access);
				when others =>
					Content := Create(To_String(Params(B + 3)));
					Registry.Query_Group(Group_Id, P'Access);
			end case;
		end; -- }}}

		procedure Process_Create (Stmt : Create_Statement; User_Id : User_Id_T; Group_Id : Group_Id_T; Result : JSON_Array) is -- {{{
			Content : Json_Value;
			Procedure P (S : in out MySocket) is
			begin
				Send_Json(S, Stmt, Content);
			end;
		begin
			case Stmt is
				when Group_Create =>
					Content := Create(Result);
					Registry.Query_User(User_Id, P'Access);
				when group_gamemaster_add =>
					declare
						Procedure Inform_User (S : in out MySocket) is
							Params : Execute_JSON.Params_T(1 .. 3) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
						begin
							Send_Json(S, own_gms_list, Create(Execute_JSON.Execute(own_gms_list, Params)));
						end;
						procedure Inform_Group (S : in out MySocket) is
							Params : Execute_JSON.Params_T(1 .. 3) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
						begin
							Send_Json(S, group_gms_list, Create(Execute_JSON.Execute(group_gms_list, Params)));
						end;
					begin
						Registry.Query_User(User_Id, Inform_User'Access);
						Registry.Query_Group(Group_Id, Inform_Group'Access);
					end;
				when character_create =>
					declare
						Procedure Inform_User (S : in out MySocket) is
							Params : Execute_JSON.Params_T(1 .. 3) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
						begin
							Send_Json(S, own_characters_list, Create(Execute_JSON.Execute(own_characters_list, Params)));
						end;
						New_Char_ID : Unbounded_String := To_Unbounded_String(String'(Get(Get(Result, 1))));
						procedure Inform_Group (S : in out MySocket) is
							Params : Execute_JSON.Params_T(1 .. 4) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S, New_Char_ID);
						begin
							Send_Json(S, character_view, Create(Execute_JSON.Execute(character_view, Params)));
						end;
					begin
						Registry.Query_User(User_Id, Inform_User'Access);
						Registry.Query_Group(Group_Id, Inform_Group'Access);
					end;
				when others =>
					case Stmt is
						when wiki_create_with_permission =>
							declare
								procedure P (S : in out MySocket) is
									Return_Result : JSON_Array;
									New_Params : Execute_JSON.Params_T(1 .. 3) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
								begin
									log ("[process_create.others.wiki_create_with_permission]", "Calling Tag_List", Debug);
									Return_Result := Execute_JSON.Execute(Tag_List, New_Params);
									Send_Json(S, Tag_List, Create(Return_Result));
								exception
									when No_Lines_Found =>
										log("[process_create.others.wiki_create_with_permission]", "No Tags found.", Debug);
								end;
							begin
								Registry.Query_Group(Group_Id, P'Access);
							end;
						when others =>
							null;
					end case;

					declare
						Answer : View_Statement := (
							case Stmt is
								when character_gallery_create_with_permission | group_gallery_create_with_permission => gallery_view,
								when appointment_create => appointment_view,
								when image_create_with_permission => image_view,
								when history_create_with_permission => history_view,
								when wiki_create_with_permission => wiki_view,
								when others => raise Constraint_Error with "Missing enum in case."
						);
						procedure P (S : in out MySocket) is
							Return_Result : JSON_Array;
							Params : Execute_JSON.Params_T(1 .. Integer(Statement_Parameters(Answer).Parameters));
						begin
							pragma assert(Params'Length = 4);
							Params(1 .. 3) := (S.User_Id_S, S.Group_Id_S, S.Character_Id_S);
							Params(4) := To_Unbounded_String(String'(Get(Get(Result, 1))));
							log ("[process_create.others]", "Stmt: " & Statement'Image(Answer) & ", Supplied Parameters: " & Parameters_To_String(Params), Debug);
							Return_Result := Execute_JSON.Execute(Answer, Params);
							Send_Json(S, Answer, Create(Return_Result));
						exception
							when No_Lines_Found =>
								log("[process_create.others]", "Nothing found.", Debug);
						end;
					begin
						Registry.Query_Group(Group_Id, P'Access);
					end;
			end case;
		end; -- }}}
	begin
		loop
			log("[listener]", "Waiting.", Debug);
			Notification_Queue.Dequeue(Data);
			declare
				Element : Queue_Data := Queue_Data_Holders.Element(Data);
			begin
				case Element.Stmt is
					when Edit_Statement =>
						log("[listener]", "Processing Edit_Statement.", Debug);
						Process_Edit(Element.Stmt, Params_T_Holders.Element(Element.Params));
					when Delete_Statement =>
						log("[listener]", "Processing Delete_Statement.", Debug);
						Process_Delete(Element.Stmt, Params_T_Holders.Element(Element.Params));
					when Create_Statement =>
						log("[listener]", "Processing Create_Statement.", Debug);
						Process_Create(Element.Stmt, Element.User_Id, Element.Group_Id, Element.Result);
					when others =>
						null;
				end case;
			exception
				when Error : others =>
					log_exception(Error, "[listener]");
			end;
		end loop;
	end Listener;

	protected body Registry is
		procedure Send_Json (S : in out MySocket; Stmt : Incoming_Messages; Content : Json_Value) is
		begin
			Send_Json(S, String'(Incoming_Messages'Image(Stmt)), Content);
		end;

		-- General Purpose
		procedure Delete_Socket_From_User (S : in out MySocket) is
			D : Boolean := False;
			S_A : constant MySocket_Access := S'Unchecked_Access;
		begin
			-- Do this, because of not tampering with the Map through Constant Index
			declare
				V : User_Maps.Reference_Type := User_Map(S.User_Id);
				I : Positive := Find_Index(V, S_A);
			begin
				V.Delete(I);
				if Length(V) = 0 then
					D := True;
				end if;
			end;

			if D then
				log("[registry.delete_socket_from_user]", "Deleting Vector from User_Map", Debug);
				User_Map.Delete(S.User_Id);
			end if;
		exception
			when Error : Constraint_Error =>
				log_exception(Error, "[registry.delete_socket_from_user]");
		end;

		procedure Delete_Socket_From_Group (S : in out MySocket; Group_Id : Group_Id_T) is
			DU : Boolean := False;
			S_A : MySocket_Access := S'Unchecked_Access;
		begin
			declare
				U : Group_Maps.Reference_Type := Group_Map(Group_Id);
				DV : Boolean := False;
			begin
				declare
					V : User_Maps.Reference_Type := U(S.User_Id);
					I : Positive := Find_Index(V, S_A);
				begin
					V.Delete(I);
					DV := Length(V) = 0;
				end;

				if DV then
					log("[registry.delete_socket_from_group]", "Deleting Vector from User_Map", Debug);
					U.Delete(S.User_Id);
					DU := Length(U) = 0;
				end if;
			end;

			if DU then
				log("[registry.delete_socket_from_group]", "Deleting User_Map from Group_Map", Debug);
				Group_Map.Delete(Group_Id);
			end if;
		exception
			when Error : Constraint_Error =>
				log_exception(Error, "[registry.delete_socket_from_group]");
		end;

		-- For the Query_Reader
		procedure Clean_Removable_Sockets is
		begin
			if Closed_Sockets_Vectors.Length(Closed_Sockets) = 0 then
				return;
			end if;
			log("[registry.clean_removable_sockets]", "Found removeable sockets.", debug);

			for S of Closed_Sockets loop
				Delete_Socket_From_User(S.all);
				Delete_Socket_From_Group(S.all, S.Group_Id);
				log("[registry.clean_removable_sockets]", "Removed " & To_String(S.all), Debug);
			end loop;

			Closed_Sockets_Vectors.Clear(Closed_Sockets);

			if log_level = debug then
				Log_Maps;
			end if;
		end;

		procedure Log_Maps is
		begin
			log("[registry.print_all]", "Group_Map contains:", Info);
			for U of Group_Map loop
				log("[registry.print_all]", "User_Map found.", Debug);
				for V of U loop
					log("[registry.print_all]", "Vector found.", Debug);
					for S of V loop
						log("[registry.print_all]", To_String(S.all), Info);
					end loop;
				end loop;
			end loop;

			log("[registry.print_all]", "User_Map contains:", Info);
			for V of User_Map loop
				log("[registry.print_all]", "Vector found.", Debug);
				for S of V loop
					log("[registry.print_all]", To_String(S.all), Info);
				end loop;
			end loop;
		end;

		procedure Query_Group(Group_Id : Natural; Process : not null access procedure (S : in out MySocket)) is
		begin
			log("[registry.query_group]", "Group_Id:" & Natural'Image(Group_Id), Debug);
			-- careful with leaks
			-- look with gdb, if internal counters of Group_Map are restored
			if Group_Maps.Contains(Group_Map, Group_Id) then
				for V of Group_Map(Group_ID) loop
					for E of V loop
						begin
							Process(E.all);
						exception
							when Socket_Error =>
								log("[registry.query_group]", "Socket seems to be closed.", Debug);
								Closed_Sockets.Append(E);
						end;
					end loop;
				end loop;
			end if;
			Clean_Removable_Sockets;
		end;

		procedure Query_User(User_Id : Natural; Process : not null access procedure (S : in out MySocket)) is
		begin
			log("[registry.query_user]", "User_Id:" & Natural'Image(User_Id), Debug);
			-- careful with leaks
			-- look with gdb, if internal counters of User_Map are restored
			if User_Maps.Contains(User_Map, User_Id) then
				log("[registry.query_user]", "Found user", Debug);
				for E of User_Map(User_Id) loop
					begin
						Process(E.all);
					exception
						when Socket_Error =>
							log("[registry.query_user]", "Socket seems to be closed.", Debug);
							Closed_Sockets.Append(E);
					end;
				end loop;
			else
				log("[registry.query_user]", "Did not found user.", Debug);
			end if;
			Clean_Removable_Sockets;
		end;

		procedure Query_All_Users(Process : not null access procedure (S : in out MySocket)) is
		begin
			log("[registry.query_all_users]", "", Debug);
			-- careful with leaks
			-- look with gdb, if internal counters of User_Map are restored
			for V of User_Map loop
				for E of V loop
					begin
						Process(E.all);
					exception
						when Socket_Error =>
							log("[registry.query_all_users]", "Socket seems to be closed.", Debug);
							Closed_Sockets.Append(E);
					end;
				end loop;
			end loop;
			Clean_Removable_Sockets;
		end;

		-- For the Sockets --
		procedure Enter_User (S : in out MySocket) is
			S_A : MySocket_Access := S'Unchecked_Access;
			C : User_Maps.Cursor := User_Map.Find(S.User_Id);
		begin
			log("[registry.enter_user]", "User: " & To_String(S.User_Id_S), Debug);
			log("[registry.enter_user]", "Previous to entering:", debug);
			Log_Maps;

			-- a user can open the page multiple times, so simply add a websocket.
			if C = User_Maps.No_Element then
				declare
					-- V : Open_Sockets_Vector_Access := new Open_Sockets_Vectors.Vector;
					Inserted : Boolean;
				begin
					User_Map.Insert (S.User_Id, C, Inserted);
				end;
				log("[registry.enter_user]", "Created vector as preparation.", Debug);
			end if;

			User_Map(C).Append(S_A);
			log("[registry.enter_user]", "Inserted.", Debug);

			log("[registry.enter_user]", "After entering:", debug);
			Log_Maps;
		end;

		procedure Enter_Group (S : in out MySocket) is
			S_A : MySocket_Access := S'Unchecked_Access;
			GMC : Group_Maps.Cursor := Group_Map.Find(S.Group_Id);
		begin
			log("[registry.enter_group]", To_String(S), Debug);
			log("[registry.enter_group]", "Previous to entering:", debug);
			Log_Maps;

			if GMC /= Group_Maps.No_Element then
				log("[registry.enter_group]", "Group_Map contains the group.", Debug);
				declare
					UM : Group_Maps.Reference_Type := Group_Map(GMC);
					UMC : User_Maps.Cursor := UM.Find(S.User_Id);
					Inserted : Boolean;
				begin
					-- Group_Map.Update_Element(GMC, Q'Access);
					if UMC = User_Maps.No_Element then
						UM.Insert(S.User_Id, UMC, Inserted);
						log("[registry.enter_group]", "Inserted new Vector to User_Map of Group.", Debug);
					end if;
					UM(UMC).Append(S_A);

					log("[registry.enter_group]", "Inserted.", Debug);
				end;
			else
				declare
					UMC : User_Maps.Cursor;
					Inserted : Boolean;
				begin
					log("[registry.enter_group]", "Group_Map does not contain the group.", Debug);
					Group_Map.Insert(S.Group_Id, GMC, Inserted);
					declare
						UM : Group_Maps.Reference_Type := Group_Map(GMC);
					begin
						UM.Insert(S.User_Id, UMC, Inserted);
						UM(UMC).Append(S_A);
					end;
				end;
			end if;

			log("[registry.Enter_Group]", "After entering:", debug);
			Log_Maps;
		end;

		procedure Switch_Group (S : in out MySocket; Old_Group_Id : Group_Id_T) is
			S_A : MySocket_Access := S'Unchecked_Access;
		begin
			log("[registry.Switch_Group]", To_String(S), Debug);

			log("[registry.Switch_Group]", "Previous to switching:", debug);
			Log_Maps;

			Delete_Socket_From_Group(S, Old_Group_Id);
			
			Enter_Group(S);

			log("[registry.Switch_Group]", "After switching:", debug);
			Log_Maps;
		end;

		procedure Delete (S : in out MySocket) is
		begin
			log("[registry.delete]", To_String(S), Debug);
			log("[registry.delete]", "Previous to deletion:", debug);
			Log_Maps;

			Delete_Socket_From_Group(S, S.Group_Id);
			Delete_Socket_From_User(S);

			log("[registry.delete]", "After deletion:", debug);
			Log_Maps;
		end;

		procedure Logout (S : in out MySocket) is
			procedure P (Socket : in out MySocket) is
			begin
				if Socket.Id /= S.Id then
					Send_Json(Socket, Logout, Create);
				end if;
			end;
		begin
			Query_User(S.User_Id, P'Access);
			-- for E of User_Map(S.User_Id) loop
				-- if E.all.Id /= S.Id then
					-- Send_Json(E.all, Logout, Create);
				-- end if;
			-- end loop;

			-- Delete(S);
		end;

	end Registry;

	-- maybe it is possible to the notify stuff
	procedure Notify (Stmt : Notifiable_Statement; Params : Params_T; Result : JSON_Array) is
	begin
		case Stmt is
			when Edit_Statement | Delete_Statement =>
				log("[websocket.notify]", "Enqueuing Edit_Delete", Debug);
				Notification_Queue.Enqueue(
					To_Holder((Kind => Edit_Delete, Stmt => Stmt, Params => To_Holder(Params)))
				);
			when Create_Statement =>
				log("[websocket.notify]", "Enqueuing Create", Debug);
				case Stmt is
					when group_gamemaster_add | character_create =>
						Notification_Queue.Enqueue(
							To_Holder((
								Kind => Create,
								Stmt => Stmt,
								Result => Result,
								Group_Id => Group_Id_T'Value(To_String(Params(Params'First + 1))),
								User_Id => User_Id_T'Value(To_String(Params(Params'First + 3)))
							))
						);
					when Group_Create =>
						Notification_Queue.Enqueue(
							To_Holder((
								Kind => Create,
								Stmt => Stmt,
								Result => Result,
								Group_Id => No_Group_Id,
								User_Id => User_Id_T'Value(To_String(Params(Params'First)))
							))
						);
					when others =>
						Notification_Queue.Enqueue(
							To_Holder((
								Kind => Create,
								Stmt => Stmt,
								Result => Result,
								Group_Id => Group_Id_T'Value(To_String(Params(Params'First + 1))),
								User_Id => No_User_Id
							))
						);
				end case;
			when others =>
				raise Program_Error with "[WebSocket.Notify] It is not allowed to call this with any other Statement (" & Statement'Image(Stmt) & ").";
		end case;
	end;
end;
