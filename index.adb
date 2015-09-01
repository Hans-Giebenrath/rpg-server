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
with Ada.Unchecked_Deallocation; 
with Util; use Util;

package body index is

	function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data is
		 WWW_Root : String renames Config.WWW_Root
		 (Server.Config (Server.Get_Current.all));
		 Filename : constant String := WWW_Root & "index.html";
	begin
		return Response.File(
			Content_Type => MIME.Text_HTML,
			Filename => Filename
		);
	end HW_CB;
end index;
