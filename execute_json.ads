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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnatcoll.Json; use Gnatcoll.Json;
with Db.Stmts; use Db.Stmts;

package Execute_JSON is
	type Params_T is array (Integer range <>) of Unbounded_String;
	function execute (Statement_To_Execute : Ajax_Statement; Params : Params_T; Reraise_No_Lines_Found : Boolean := True) return JSON_Array;
end;
