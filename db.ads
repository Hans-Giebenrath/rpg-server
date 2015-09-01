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

-- this file is a modified variant of gnatcoll's gnatcoll-sql-postgres-gnade

with Ada.Finalization;
with Ada.Unchecked_Deallocation; 
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package db is
	type OID is new Interfaces.C.unsigned;
	type Permission is (NONE, READ, MODIFY, DELETE, ROOT);
	for Permission use (NONE => 0, READ => 100, MODIFY => 200, DELETE => 300, ROOT => 400);
	function Permission_Image(P : Permission) return String is
		(case P is
			when NONE => "0",
			when READ => "100",
			when MODIFY => "200",
			when DELETE => "300",
			when ROOT => "400");
	--  PostreSQL assigns a unique Object ID (OID) to a row.

	InvalidOID : constant OID;
	--  This denotes an invalid OID.

	type param_lengths_t is array (Integer range <>) of aliased int;
	type param_formats_t is array (Integer range <>) of aliased int;
	type param_types_t is array (Integer range <>) of aliased OID;
	type param_values_t is new chars_ptr_array;
	No_Param_Values : aliased param_values_t (1 .. 0) := (others => Null_Ptr);
	type Param_Values_T_Access is access Param_Values_T;
	procedure Free is new Ada.Unchecked_Deallocation(Param_Values_T, Param_Values_T_Access);
	function To_Chars_Ptr(S : String) return chars_ptr is
		(if S = "" then null_ptr else New_String(S));

	type PGconnection is private;
	Null_Connection : constant PGconnection;

	No_Lines_Found : exception;
private
	-- use Ada.Finalization;

	InvalidOID : constant OID := 0;

	type PGresult is new System.Address;
	Null_Result : constant PGresult := PGresult (System.Null_Address);

	type PGconnection is new System.Address;
	Null_Connection : constant PGconnection := PGconnection (System.Null_Address);
	-- No_Param_Values : constant param_values_t (1 .. 0);
	-- will throw an exception and clears the result, if something went wrong.
	procedure Check_For_Errors (result : PGresult);

	function Row_To_Unbounded (Res : DB.PGresult; Tuple : Natural; Field : Natural) return Unbounded_String;
	Pragma Inline(Row_To_Unbounded);
end db;
