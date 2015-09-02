package Config is
	-- the script shall take the arguments as follows:
	-- <full source path> <full target path> <resolution>
	-- Keep in mind, that it is executable.
	function Image_Conversion_Script return String;

	-- Something like aws-server.key. Used for SSL.
	function Private_Key_Name return String;
	-- Something like aws-server.cert. Used for SSL.
	function Certificate_Name return String;

	-- Port of the Server
	function Server_Port return Natural;
	function Server_Name return String;

	function Psql_Hostname return String;
	function Psql_Port return String;
	function Psql_Username return String;
	function Psql_Database return String;
	-- Num of parallel connections
	function Psql_Connections return Natural;
private
	pragma Inline(Image_Conversion_Script);
	pragma Inline(Private_Key_Name);
	pragma Inline(Certificate_Name);
	pragma Inline(Server_Port);
	pragma Inline(Server_Name);
	pragma Inline(Psql_Hostname);
	pragma Inline(Psql_Port);
	pragma Inline(Psql_Username);
	pragma Inline(Psql_Database);
	pragma Inline(Psql_Connections);
end;
