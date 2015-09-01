with AWS.Response;
with AWS.Status;

package static is

   function HW_CB
     (Request : AWS.Status.Data)
      return AWS.Response.Data;

end static;
