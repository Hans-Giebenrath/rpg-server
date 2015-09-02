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

with ada.containers.vectors; use ada.containers;

package db.connections is
	function Reserve_Connection return PGconnection;
	procedure Release_Connection (Connection : PGconnection);

	function initialized return boolean;
	procedure Init_Connections (Connection_String_P : String; Number_Of_Connections : Natural)
		with
			Pre => not initialized,
			Post => initialized;
		

	-- TODO this would be cool for runtime configuration
	-- procedure Increment_Number_of_Parallel_Connections;
	-- procedure Decrement_Number_of_Parallel_Connections;
	procedure Set_Number_Of_Parallel_Connections (Number_Of_Parallel_Connections : Natural);
	function Get_Number_Of_Parallel_Connections return Natural;

	procedure Check_Connection_Status (Connection : PGconnection);

	procedure Query_Connections (Proc : not null access procedure (Connection : PGconnection));

	-- TODO Create callbacks to register, if a connection will be established
	-- why? ...
end db.connections;
