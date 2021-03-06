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

with Util; use Util;
with Config;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Ordered_Maps;
with System;

package body Image_Handling is

	Private_Image_Dir : constant String := "private" & Dir_Separator & "image" & Dir_Separator;
	Public_Image_Dir : constant String := "static" & Dir_Separator & "image" & Dir_Separator;

	package Queue_Data_Holders is new Ada.Containers.Indefinite_Holders(String);
	use Queue_Data_Holders;

	package Filename_Queue_Interface is
		new Synchronized_Queue_Interfaces(Element_Type => Queue_Data_Holders.Holder);

	package Filename_Queues is
		new Unbounded_Synchronized_Queues(Queue_Interfaces => Filename_Queue_Interface);
	use Filename_Queues;

	Filename_Queue : Filename_Queues.Queue;

	Task Image_Resizer
		with Priority => System.Priority'First;
	Task Body Image_Resizer is
		Data : Queue_Data_Holders.Holder;
	begin
		loop
			Filename_Queue.Dequeue(Data);
			declare
				Filename : constant String := Element(Data);
				Old_Name : constant String := Private_Image_Dir & "confirmed" & Dir_Separator & Filename;
				F : constant Integer := Random_Filename_T'First;
				New_Path : constant String := Public_Image_Dir & Filename(F .. F + 1) & Dir_Separator & Filename(F + 2 .. F + 3);
				New_Name : constant String := New_Path & Dir_Separator & Filename;
				Executable : constant String := Config.Image_Conversion_Script;
				-- NOTE take care, that no spaces are in paths. Or escape with backslash.
				Small_Command : constant String := Executable & " " & Old_Name & " " & New_Name & ".small 200x200";
				Medium_Command : constant String := Executable & " " & Old_Name & " " & New_Name & ".medium 600x600";
				Args : Argument_List_Access;
				Exit_Status : Integer;
			begin
				Create_Path(New_Path);
				Args := Argument_String_To_List (Small_Command);
				-- small image
				log("[image_resizer]", "Executing Command: """ & Small_Command & """", Debug);
				Exit_Status := Spawn
					(Program_Name => Args (Args'First).all,
					Args         => Args (Args'First + 1 .. Args'Last));
				log("[image_resizer]", "Exit_Status:" & Integer'Image(Exit_Status), Debug);
				Free(Args);

				Args := Argument_String_To_List (Medium_Command);
				-- medium image
				log("[image_resizer]", "Executing Command: """ & Medium_Command & """", Debug);
				Exit_Status := Spawn
					(Program_Name => Args (Args'First).all,
					Args         => Args (Args'First + 1 .. Args'Last));
				log("[image_resizer]", "Exit_Status:" & Integer'Image(Exit_Status), Debug);
				Free(Args);

				Rename (
				-- Copy_File (
					Old_Name => Old_Name,
					New_Name => New_Name & ".orig"
				);
			end;
		end loop;
	end;

	procedure Process_Image(Random_Filename : Random_Filename_T) is
		Filename : constant Random_Filename_T := Simple_Name(Random_Filename);
	begin
		Rename (
			Old_Name => Private_Image_Dir & "pending" & Dir_Separator & Filename,
			New_Name => Private_Image_Dir & "confirmed" & Dir_Separator & Filename
		);
		Filename_Queue.Enqueue(To_Holder(Filename));
	end;

	procedure Delete_Image(Filename : Random_Filename_T) is
		F : constant integer := Random_Filename_T'First;
		Path : constant String := Public_Image_Dir & Filename(F .. F + 1) & Dir_Separator & Filename(F + 2 .. F + 3);
		Name : constant String := Path & Dir_Separator & Filename;
	begin
		if Exists (Name & ".orig") then
			log("[delete_image]", "Deleting " & Name & ".orig", Debug);
			Delete_File (Name & ".orig");
		else
			log("[delete_image]", "File " & Name & ".orig not found", warning);
		end if;

		if Exists (Name & ".small") then
			log("[delete_image]", "Deleting " & Name & ".small", Debug);
			Delete_File (Name & ".small");
		else
			log("[delete_image]", "File " & Name & ".small not found", warning);
		end if;

		if Exists (Name & ".medium") then
			log("[delete_image]", "Deleting " & Name & ".medium", Debug);
			Delete_File (Name & ".medium");
		else
			log("[delete_image]", "File " & Name & ".medium not found", warning);
		end if;
	end;
end;
