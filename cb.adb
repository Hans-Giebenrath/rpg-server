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

with AWS.Status; use AWS.Status;
with AWS.Response; use AWS.Response;
with Ajax;
with Role;
with Upload;
with Static;
with Status;
with Index;
function CB (Request : AWS.Status.Data) return AWS.Response.Data is
	Orig : constant String := URI (Request);
	F : constant Integer := Orig'First;
	L : constant Integer := Orig'Length;
begin
   if L >= 6 and then Orig(F .. F + 5) = "/ajax/" then
	   return Ajax.HW_CB(Request);
   elsif L >= 6 and then Orig(F .. F + 5) = "/role/" then
	   return Role.HW_CB(Request);
   elsif L >= 8 and then Orig(F .. F + 7) = "/upload/" then
	   return Upload.HW_CB(Request);
   elsif L >= 8 and then Orig(F .. F + 7) = "/static/" then
	   return Static.HW_CB(Request);
   elsif L >= 8 and then Orig(F .. F + 7) = "/status/" then
	   return Status.HW_CB(Request);
   else
	   return Index.HW_CB(Request);
   end if;
end;
