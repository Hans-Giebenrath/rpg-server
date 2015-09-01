with AWS.Response;
with AWS.Status;

package Status is

   function HW_CB
     (Request : AWS.Status.Data)
      return AWS.Response.Data;

end Status;
