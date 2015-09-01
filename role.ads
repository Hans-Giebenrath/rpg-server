with AWS.Response;
with AWS.Status;

package role is

   function HW_CB
     (Request : AWS.Status.Data)
      return AWS.Response.Data;

end role;

