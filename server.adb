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
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Ada.Text_IO;

with AWS.Config;
with AWS.Config.Set;
with AWS.Server;
with AWS.Server.Log;
with AWS.Services.Dispatchers.URI;
with AWS.Net.WebSocket.Registry;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Net.Log;
with AWS.Net.Std;
with AWS.Net.SSL;
with AWS.Mime;
use AWS;

with Config;--  use Config;

with Ada.DIrectories; use Ada.Directories;

with WebSocket;

with db.stmts; use db.stmts;
with util; use util;
with AWS.Status; use AWS.Status;
with AWS.Response; use AWS.Response;
with cb;

procedure Server is

	use Ada;
	use AWS.Services;

	-- Rcp : Net.WebSocket.Registry.Recipient :=                                         
		-- Net.WebSocket.Registry.Create (URI => "/ws");

	C : AWS.Config.Object := AWS.Config.Get_Current;
begin
	log("[server]", "Creating Paths", Info);
	Create_Path("private/image/pending");
	Create_Path("private/image/confirmed");
	Create_Path("static/image");

	AWS.Config.Set.Session_Lifetime(86400.0);
	AWS.Config.Set.Security_Mode(C, "TLSv1_2");
	AWS.Mime.Add_Extension("svg", "image/svg+xml");

	log("[server]", "Initializing database connections ...", Info);
	Init(
		"user=" & Config.Psql_Username & 
		" dbname=" & Config.Psql_Database &
	    " host=" & Config.Psql_Hostname &
	    " port=" & Config.Psql_Port, Config.Psql_Connections);

	declare
		WS : AWS.Server.HTTP;
	begin
		if Config.Log_Request then
			Aws.Server.Log.Start(WS, Auto_Flush => True);
			Aws.Server.Log.Start_Error(WS);
		end if;

		log("[server]", "Setting Security", Info);

		if not AWS.Net.SSL.Is_Supported then
			log("[server]", "SSL not supported.", emergency);
			return;
		end if;

		AWS.Server.Set_Security (
			WS,
			Key_Filename         => Config.Private_Key_Name,
			Certificate_Filename => Config.Certificate_Name);

		log("[server]", "Starting server on port " & Natural'Image(Config.Server_Port), Info);
		AWS.Server.Start (
			Web_Server => WS,
			Callback => CB'Access,
			Session => True,
			Name => Config.Server_Name,
			Port => Config.Server_Port,
			Security => True,
			Upload_Directory => "private/image"
		);

		log("[server]", "Starting WebSocket.Registry", Info);
		AWS.Net.WebSocket.Registry.Control.Start;

		log("[server]", "Registering /ws (websocket entry)", Info);
		AWS.Net.WebSocket.Registry.Register ("/ws", WebSocket.Create'Access);

		--  Wait for 'q' key pressed...

		log("[server]", "AWS " & AWS.Version, info);
		log("[server]", "Enter 'q' key to exit... and Ctrl-C", info);
		AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

		--  Close servers.

		AWS.Server.Shutdown (WS);
	end;
end Server;
