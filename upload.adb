with Ada.Directories; use Ada.Directories;

with AWS.MIME;
with AWS.Parameters;
with Gnatcoll.Json; use Gnatcoll.Json;
with Ada.Strings.Hash;
with util; use util;
with Ada.Numerics.discrete_Random;
with AWS.Containers.Tables; use AWS.Containers.Tables;
with AWS.Session; use AWS.Session;
with AWS.Status; use AWS.Status;

package body Upload is
	-- NOTE the images will be put into "private/image/pending/<Random_File_Name>"
	-- The random file name will be returned to the browser, where now the final ajax
	-- request is triggered -> Image_Handling.adb
	-- This means: moving the image to "private/image/confirmed/<Simple_Filename(<Random_File_Name>)>.orig" where
	-- it waits for processing.
	-- After processing the files are copied to
	-- "static/img/<first two digits>/<next two digits>/<Random_File_Name>.(orig|small|medium)"
	-- Afterwards the internal image-resizing queue is fed. Note: there is no internal
	-- householding of the file type (jpg, bmp, gif, png, ...).

	use Ada;

	-----------
	-- HW_CB --
	-----------

	function HW_CB (Request : Status.Data) return Response.Data is
		URI : constant String          := Status.URI (Request);
		P   : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
		S : constant AWS.Session.ID := AWS.Status.Session (Request);
		File_Name : String := AWS.Parameters.Get(P, "filename");
		Random_File_Name : Random_Filename_T := Generate_Random_String;
		procedure Iter (Name, Value: String) is
		begin
			log("[upload.iter]", "Name: " & Name & ", Value: " & Value, Debug);
		end;
	begin
		if not aws.session.exist(S, "user_id") then
			log ("[/upload/]", "Not logged in. URL: " & URL(Request) & ", Headers: " & Headers_To_String(Header(Request)), emergency);
			return Json_To_Response(Error_Json ("You are not logged in."));
		end if;
		-- log("[upload]", "Num of Parameters: " & Natural'Image(Count(P)), Debug);
		log("[/upload/]", "File Name: " & File_Name & ", Hashed: " & Random_File_Name, Debug);
		Rename (File_Name, "private" & Dir_Separator & "image" & Dir_Separator & "pending" & Dir_Separator & Random_File_Name);

		return Json_To_Response(Success_Json(Create(Random_File_Name)));
	end HW_CB;

end Upload;
