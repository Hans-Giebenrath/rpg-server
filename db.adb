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

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;          use System;

package body db is

	-- subtype char_array is char_array;

	procedure Check_For_Errors (result : PGresult) is
		function PQresStatus (Res : PGresult) return Interfaces.C.int;
		pragma Import (C, PQresStatus, "PQresultStatus");
		type ExecStatus is (
		--  Direct mappings for C values
			PGRES_EMPTY_QUERY,
			PGRES_COMMAND_OK,
			PGRES_TUPLES_OK,
			PGRES_COPY_OUT,
			PGRES_COPY_IN,
			PGRES_BAD_RESPONSE,
			PGRES_NONFATAL_ERROR,
			PGRES_FATAL_ERROR,
			PGRES_COPY_BOTH
		);
		pragma Convention (C, ExecStatus);

		function PQresErr (Res : PGresult) return chars_ptr;
		pragma Import (C, PQresErr, "PQresultErrorMessage");

		Status : ExecStatus := ExecStatus'Val(PQresStatus(result));
	begin
		case Status is
			when PGRES_COMMAND_OK | PGRES_TUPLES_OK =>
				null;
			when others =>
				raise program_error with
					"The query did not return a valid result. (" &
					ExecStatus'Image(Status) & ")" &
					Value(PQresErr(result));
		end case;
	end Check_For_Errors;

	function Row_To_Unbounded (Res : DB.PGresult; Tuple : Natural; Field : Natural) return Unbounded_String is
		function PQgetvalue (Res   : PGresult;
						   Tuple : int;
						   Field : int) return chars_ptr;
		pragma Import (C, PQgetvalue, "PQgetvalue");
	begin
		return To_Unbounded_String(Value(PQgetValue(Res, int(Tuple), int(field))));
	end Row_To_Unbounded;
end db;
