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

with Ada.Calendar; use Ada.Calendar;
with AWS.Response; use AWS.Response;
with AWS.Messages; use AWS;
with AWS.Status; use AWS.Status;
with AWS.Parameters;
with AWS.Containers.Tables;
with Ada.Numerics.discrete_Random;
with Gnatcoll.Json; use Gnatcoll.Json;
with Ada.Exceptions;
with Ada.Text_IO; Use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Execute_Json; use Execute_Json;
with AWS.Headers; use AWS.Headers;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gnat.Calendar.Time_IO; use Gnat.Calendar.Time_IO;

package body Util is
	Log_File : File_Type;

	function Exception_To_String (E : Ada.Exceptions.Exception_Occurrence; Prefix : String := "") return String is
		(Prefix & ", Exception caught: " & Ada.Exceptions.Exception_Name(E) & ": " & Ada.Exceptions.Exception_Message(E));

	function Cut_First_Char (S : String) return String is
		(S(S'First + 1 .. S'Last));

	function Log_File_Name return String is
		Year : Year_Number;
		Month : Month_Number;
		Day : Day_Number;
		Seconds : Day_Duration;
		function C (S : String) return String renames Cut_First_Char;
	begin
		Split(Clock, Year, Month, Day, Seconds);
		return "log_" & C(Year_Number'Image(Year)) & "-" & C(Month_Number'Image(Month)) & "-" & C(Day_Number'Image(Day)) & "-" & C(Day_Duration'Image(Seconds));
	end;

	procedure log (Prefix : String; Message : String; Level : Log_Level_Type) is
		SD_EMERG : constant String := "<0>";
		SD_ALERT : constant String := "<1>";
		SD_CRIT : constant String := "<2>";
		SD_ERR : constant String := "<3>";
		SD_WARNING : constant String := "<4>";
		SD_NOTICE : constant String := "<5>";
		SD_INFO : constant String := "<6>";
		SD_DEBUG : constant String := "<7>";

		SD : String(1..3);
	begin
		if Level > Log_Level then                                                   
			return;
		end if; 
		
		case Level is
			when none => return;
			when emergency => SD := SD_EMERG;
			when alert => SD := SD_ALERT;
			when critical => SD := SD_CRIT;
			when error => SD := SD_ERR;
			when warning => SD := SD_WARNING;
			when notice => SD := SD_NOTICE;
			when info => SD := SD_INFO;
			when debug => SD := SD_DEBUG;
		end case;

		declare
			T : String := SD & " " & Prefix & " " & Message & " [" & Image(Clock, Picture_String'("%Y - %m - %d, %T, %e")) & "]";
		begin
			Put_Line(T);
			Put_Line(Log_File, T);
			Flush(Log_File);
		end;
	end log;

	procedure log_exception(E : Ada.Exceptions.Exception_Occurrence; Prefix : String; Level : Log_Level_Type := Error)  is
	begin                                                                            
		log(Prefix, "Exception caught: " & Ada.Exceptions.Exception_Name(E) & ": " & Ada.Exceptions.Exception_Message(E), Level);
	end log_exception;

	function Result_Json (Msg : String; Payload : Json_Value; Success : Boolean) return Json_Value is
	begin
		return Result : Json_Value := Create_Object do
			Result.Set_Field("success", Create(Success));

			if Msg /= "" then
				Result.Set_Field("msg", Create(Msg));
			end if;

			if Payload /= JSON_Null then
				Result.Set_Field("data", Payload);
			end if;
		end return;
	end Result_Json;

	function Error_Json (Msg : String) return Json_Value is
		(Result_Json(Msg, JSON_Null, false));

	function Success_Json (Payload : Json_Value := JSON_Null) return Json_Value is
		(Result_Json("", Payload, true));

	function Json_To_Response (Payload : Json_Value) return AWS.Response.Data is
		(if Get(Payload, "success") then
			AWS.Response.Build ("text/html", Message_Body => Write(Get(Payload, "data")), Status_Code => Messages.S200, Encoding => Messages.Gzip)
		else
			AWS.Response.Build ("text/html", Message_Body => Write(Get(Payload, "msg")), Status_Code => Messages.S406, Encoding => Messages.Gzip)
		);

	function Parameters_To_String (P : Params_T) return String is
		Buffer : Unbounded_String;
		procedure tmp (Value : Unbounded_String) is
		begin
			Append(Buffer, To_String(Value) & "; ");
		end tmp;
	begin
		for E of P loop
			tmp(E);
		end loop;
		return To_String(Buffer);
	end;

	function Parameters_To_String (P : AWS.Parameters.List) return String is
		Buffer : Unbounded_String;
		procedure tmp (Key, Value : String) is
		begin
			Append(Buffer, "Key: " & Key);
			Append(Buffer, ", Value: " & Value & "; ");
		end tmp;
	begin
		-- AWS.Containers.Tables.Iterate_Names(P, "", tmp);
		AWS.Parameters.Iterate_Names(P, "", tmp'Access);
		return To_String(Buffer);
	end;

	function Natural_To_String (N : Natural) return String is
		Result : String := Natural'Image(N);
	begin
		return Cut_First_Char(Result);
	end Natural_To_String;

	-- only generate small numbered stuff. Those will be enough.
	Chars : constant String := "0123456789abcdefghijklmnopqrstuvwxyz";
	subtype Rand_Range is Positive range Chars'First .. Chars'Last;
	package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);

	gen : Rand_Int.Generator;

	function generate_random_number return Integer is
	begin
		return Rand_Int.Random(gen);  -- or mod n+1 to include the end value
	end generate_random_number;

	function generate_random_string (n : in Positive) return String is
	begin
		return S : String (1 .. n) do
			for I in S'Range loop
				S(I) := Chars(generate_random_number);
			end loop;
		end return;
	end;

	-- function generate_random_string  return Random_Filename_T is
	-- begin
		-- return S : Random_Filename_T do
			-- for I in S'Range loop
				-- S(I) := Chars(generate_random_number);
			-- end loop;
		-- end return;
	-- end;

	function Headers_To_String (H : AWS.Headers.List) return String is
		Buffer : Unbounded_String;
		L : Natural := AWS.Headers.Count(H);
	begin
		if L = 0 then
			return "";
		end if;

		Ada.Strings.Unbounded.Append(Buffer, Get_Line(H, 1));

		for I in 2 .. L loop
			Ada.Strings.Unbounded.Append(Buffer, HT & Get_Line(H, I));
		end loop;

		return To_String(Buffer);
	end;
begin
	Rand_Int.Reset(gen);
	Create(File => Log_File, Mode => Out_File, Name => Log_File_Name);
end Util;
