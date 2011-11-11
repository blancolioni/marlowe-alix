with Ada.Text_IO;

package body Marlowe.Trace is

   -----------
   -- Trace --
   -----------

   procedure Trace (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Message);
      Ada.Text_IO.Flush;
   end Trace;

end Marlowe.Trace;
