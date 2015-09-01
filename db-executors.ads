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

with Ada.Iterator_Interfaces;
with db.stmts; use db.stmts;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Db.Executors is
	type Void (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) is new Ada.Finalization.Limited_Controlled with private;
	-- type Result_Set_Access is access Result_Set'Class;

	procedure Execute (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement);

	type Row_Data is array (Positive range <>) of aliased Unbounded_String;
	type Row_Data_Access is access Row_Data;
	type Row_Data_Reference (Element : access Unbounded_String) is limited private
		with
			Implicit_Dereference => Element;

	type Single (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) is new Void with private;
		-- with 
			-- Constant_Indexing => Row_Value;

	function Execute (E : in out Single) return Row_Data_Access;

	-- function Row_Value (E : in out Single'Class; Idx : Natural) return Row_Data_Reference;

	type Cursor is private;
	function Has_Element (Pos : Cursor) return Boolean;
	No_Element : constant Cursor;

	type Table (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) is new Single with private
		with
		Constant_Indexing => Row_Value,
		Default_Iterator  => Iterate,
		Iterator_Element  => Row_Data_Access;
	type Table_Access is access Table;

	function Row_Value (Container : in out Table; Pos : Cursor)
		return Row_Data_Access;

	package List_Iterator_Interfaces is new
	Ada.Iterator_Interfaces (Cursor, Has_Element);

	function Iterate (Container : Table)
		return List_Iterator_Interfaces.Forward_Iterator'Class;
private
	type Void (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) is new Ada.Finalization.Limited_Controlled with
		record
			Connection : PGconnection := Null_Connection;
			Result : PGResult := Null_Result;
		end record;

	overriding
	procedure Finalize (E : in out Void);

	-- make list controlled limited, to free PGresult on exit
	type Single (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) is new Void (Param_Values, Stmt_To_Execute) with
		record
			Active_Tuple : Natural := 0;
			Total_Tuples : Natural;
			Row : Row_Data_Access := null;
		end record;

	overriding
	procedure Finalize (E : in out Single);

	type Row_Data_Reference (Element : access Unbounded_String) is limited null record;

	-- need not to be controlled, as resources still live afterwards and
	-- gambling is not really possible
	type Iterator is limited new
		List_Iterator_Interfaces.Forward_Iterator with
		record
			Container : Table_Access;
		end record;

	overriding function First (Object : Iterator) return Cursor;

	overriding function Next
		(Object   : Iterator;
	Position : Cursor) return Cursor;

	-- make list controlled limited, to free PGresult on exit
	type Table (Param_Values : access Param_Values_T; Stmt_To_Execute : Statement) is new Single (Param_Values, Stmt_To_Execute) with null record;

	-- overriding
	-- procedure Finalize (Object : in out Table);

	type Cursor is
		record
			Element : Row_Data_Access;
		end record;

	No_Element : constant Cursor := Cursor'(Element => null);
end Db.Executors;
