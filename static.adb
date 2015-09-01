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
with AWS; use AWS;
with AWS.Status;
with AWS.Config;
with AWS.MIME;
with AWS.Server;
with AWS.Resources;
with AWS.Services.Directory;

with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Unchecked_Deallocation; 
with Util; use Util;

package body static is

	function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data is
		 WWW_Root : String renames Config.WWW_Root
		 (Server.Config (Server.Get_Current.all));
		 URI      : constant String := Status.URI (Request);
		 Filename : constant String := WWW_Root & "static/" & URI (URI'First + 8 .. URI'Last);
		 Orig_Ext : constant String := ".orig";
	begin
		if Filename'Length >= Orig_Ext'Length and then Filename (Filename'Last - Orig_Ext'Length + 1..Filename'Last) = Orig_Ext then
			-- serving an original image. Attach real filename.
			-- The filename is concatenated like following:
			-- <original-filename>:<random-filename>.orig
			declare
				Basename : String := URI (URI'First + 20 .. URI'Last);
				Splitpos : Natural := Index(Source => Basename, Pattern => ":", From => Basename'Last, Going => Backward);
				Orig_Filename : String := Basename(Basename'First .. Splitpos - 1);
				Random_Filename : String := WWW_Root & "static/image/" & URI (URI'First + 14 .. URI'First + 19) & Basename(Splitpos + 1 .. Basename'Last);
			begin
				log("[static]", "Basename: " & Basename & ", Orig_Filename: " & Orig_Filename & ", Random_Filename: " & Random_Filename, Debug);
				if Resources.Is_Regular_File(Random_Filename) then
					return Response.File(
						Content_Type => MIME.Content_Type(Orig_Filename),
						Filename => Random_Filename,
						User_Filename => Orig_Filename,
						Disposition => Response.Inline
					);
				else
					return Response.Build(
						Content_Type => MIME.Text_Plain,
						Message_Body => """File " & Random_Filename & " not found.""" -- todo remove Filename for xss
					);
				end if;
			end;
		end if;

		if Resources.Is_Regular_File(Filename) then
			-- TODO include it in production.
			-- log("[static]" , "Serving file " & Filename & " with mime-type " & MIME.Content_Type(Filename), Debug);

			return Response.File(
				Content_Type => MIME.Content_Type(Filename),
				Filename => Filename
			);
		else
			return Response.Build(
				Content_Type => MIME.Text_Plain,
				Message_Body => """File " & Filename & " not found.""" -- todo remove Filename for xss
			);
		end if;
	end HW_CB;
end static;
