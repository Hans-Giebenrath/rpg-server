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
