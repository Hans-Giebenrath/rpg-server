with ada.strings.unbounded; use ada.strings.unbounded;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with util; use util;

package body DB.Connections is
	Connection_String : aliased access char_array;
	Is_Initialized : Boolean := false;

	function initialized return boolean is
		(Is_Initialized);


	protected Connection_Semaphore is
		entry Reserve_Connection (connection : out PGconnection);
		procedure Release_Connection (Connection : PGconnection);

		entry Decrease_Connection_Count;
		procedure Increase_Connection_Count;

		function Get_Maximum_Connections return Natural;
		function Get_Current_Connections return Natural;
	private
		Maximum_Connections : Natural := 0;
		Current_Connections : Natural := 0;
	end Connection_Semaphore;

	protected body Connection_Semaphore is
		entry Reserve_Connection (Connection : out PGconnection) when Current_Connections < Maximum_Connections is
		begin
			Connection := Last_Element(Connections);
			Delete_Last(Connections);
			Current_Connections := Current_Connections + 1;

			log ("[Connection_Semaphore.Reserve_Connection] Current_Connections:", Natural'Image(Current_Connections), debug);
		end Reserve_Connection;

		procedure Release_Connection (Connection : PGconnection) is
		begin
			pragma Assert(Current_Connections > 0, "Trying to release connection in semaphore.");

			Append(Connections, Connection);
			-- decrease current connection count
			Current_Connections := Current_Connections - 1;

			log ("[Connection_Semaphore.Release_Connection] Current_Connections:", Natural'Image(Current_Connections), debug);
		end Release_Connection;

		entry Decrease_Connection_Count when Current_Connections < Maximum_Connections is
			procedure PQfinish (Connection : PGconnection);
			pragma Import (C, PQfinish, "PQfinish");

			procedure Call_PQfinish (Connection : PGconnection) is
			begin
				PQfinish(Connection);
			end Call_PQfinish;
		begin
			pragma Assert(Maximum_Connections > 0, "Trying to decrease Maximum Connections below 0.");

			-- close connection to database from last element
			Query_Element(Connections, Natural(Length(Connections)), Call_PQfinish'Access);

			-- remove element from container
			-- Also decreases length of container
			Delete_Last(Connections);

			Maximum_Connections := Maximum_Connections - 1;
		end Decrease_Connection_Count;

		procedure Increase_Connection_Count is
			function PQConnect (Options : char_array) return PGconnection;
			pragma Import (C, PQConnect, "PQconnectdb");

			Conn : PGconnection := PQconnect(COnnection_String.all);
		begin
			Check_Connection_Status(Conn);

			Append(Connections, PQconnect(Connection_String.all));
			Maximum_Connections := Maximum_Connections + 1;
		end Increase_Connection_Count;

		function Get_Maximum_Connections return Natural is
			(Maximum_Connections);

		function Get_Current_Connections return Natural is
			(Current_Connections);
	end Connection_Semaphore;

	procedure Check_Connection_Status (Connection : PGconnection) is
		function PQstatus (Conn : PGconnection) return Interfaces.C.int;
		pragma Import (C, PQstatus, "PQstatus");

		type ConnStatus is (
			CONNECTION_OK,
			CONNECTION_BAD,
			CONNECTION_STARTED,
			CONNECTION_MADE,
			CONNECTION_AWAITING_RESPONSE,
			CONNECTION_AUTH_OK,
			CONNECTION_SETENV
		);
		pragma Convention (C, ConnStatus);

		function PQerr (Conn : PGconnection) return chars_ptr;
		pragma Import (C, PQerr, "PQerrorMessage");
	begin
		case ConnStatus'Val (PQstatus (Connection)) is
			when Connection_OK =>
				null;
			when others =>
				raise Program_Error with "Connection could not be established. " & Value(PQerr(Connection));
		end case;
	end Check_Connection_Status;
			
	function Reserve_Connection return PGconnection is
	begin
		return Result : PGconnection do
			Connection_Semaphore.Reserve_Connection(Result);
		end return;
	end Reserve_Connection;

	procedure Release_Connection (Connection : PGconnection) is
	begin
		Connection_Semaphore.Release_Connection(Connection);
	end Release_Connection;

	procedure Init_Connections (Connection_String_P : String; Number_Of_Connections : Natural) is
	begin
		Connection_String := new char_array'(To_C(Connection_String_P));
		Reserve_Capacity(Connections, Count_Type(Number_Of_Connections));
		Set_Number_Of_Parallel_Connections (Number_Of_Connections);
		Is_Initialized := true;
	end Init_Connections;

	procedure Set_Number_Of_Parallel_Connections (Number_Of_Parallel_Connections : Natural) is
		Maximum_Connections : Natural := Connection_Semaphore.Get_Maximum_Connections;
		-- Current_Connections : Natural := Connection_Semaphore.Get_Current_Connections;
	begin
		-- just possible to do it step by step, as we cannot reference an entry's
		-- parameter in the entry_barrier (9.5.2 Par 18)
		if Maximum_Connections = Number_Of_Parallel_Connections then
			return;
		elsif Maximum_Connections < Number_Of_Parallel_Connections then
			for I in 1 .. Number_Of_Parallel_Connections - Maximum_Connections loop
				Connection_Semaphore.Increase_Connection_Count;
			end loop;
		else -- length > num
			for I in 1 .. Maximum_Connections - Number_Of_Parallel_Connections loop
				Connection_Semaphore.Decrease_Connection_Count;
			end loop;
		end if;
	end Set_Number_Of_Parallel_Connections;

	function Get_Number_Of_Parallel_Connections return Natural is
		(Connection_Semaphore.Get_Maximum_Connections);

	procedure Query_Connections (Proc : not null access procedure (Connection : PGconnection)) is
	begin
		for c of connections loop
			Proc.all(c);
		end loop;
	end Query_Connections;
end DB.Connections;
