with Gnatcoll.Config; use Gnatcoll.Config;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Config is
	Config : Config_Pool;

	Image_Conversion_Script_S : Unbounded_String;
	function Image_Conversion_Script return String is
		(To_String(Image_Conversion_Script_S));

	Private_Key_Name_S : Unbounded_String;
	function Private_Key_Name return String is
		(To_String(Private_Key_Name_S));

	Certificate_Name_S : Unbounded_String;
	function Certificate_Name return String is
		(To_String(Certificate_Name_S));

	Server_Port_N : Natural;
	function Server_Port return Natural is
		(Server_Port_N);

	Server_Name_S : Unbounded_String;
	function Server_Name return String is
		(To_String(Server_Name_S));

	Log_Request_B : Boolean;
	function Log_Request return Boolean is
		(Log_Request_B);

	Psql_Hostname_S : Unbounded_String;
	function Psql_Hostname return String is
		(To_String(Psql_Hostname_S));
	
	Psql_Port_S : Unbounded_String;
	function Psql_Port return String is
		(To_String(Psql_Port_S));

	Psql_Username_S : Unbounded_String;
	function Psql_Username return String is
		(To_String(Psql_Username_S));

	Psql_Database_S : Unbounded_String;
	function Psql_Database return String is
		(To_String(Psql_Database_S));

	Psql_Connections_N : Natural;
	function Psql_Connections return Natural is
		(Psql_Connections_N);
begin
	declare
		P : INI_Parser;
	begin
		Open(P, "config.ini");
		Fill(Config, P);
	end;

	Image_Conversion_Script_S := To_Unbounded_String(Config.Get("image.conversion"));

	Private_Key_Name_S := To_Unbounded_String(Config.Get("key.priv"));
	Certificate_Name_S := To_Unbounded_String(Config.Get("key.cert"));

	Server_Port_N := Natural'Value(Config.Get("server.port"));
	Server_Name_S := To_Unbounded_String(Config.Get("server.name"));
	Log_Request_B := Boolean'Value(Config.Get("server.logrequest"));

	Psql_Hostname_S := To_Unbounded_String(Config.Get("psql.hostname"));
	Psql_Port_S :=  To_Unbounded_String(Config.Get("psql.port"));
	Psql_Username_S := To_Unbounded_String(Config.Get("psql.username"));
	Psql_Database_S := To_Unbounded_String(Config.Get("psql.database"));
	Psql_Connections_N := Natural'Value(Config.Get("psql.connections"));
end;
