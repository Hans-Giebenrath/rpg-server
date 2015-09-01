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
