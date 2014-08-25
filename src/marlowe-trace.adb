with Ada.Calendar;
with Ada.Task_Identification;
with Ada.Text_IO;

package body Marlowe.Trace is

   File       : Ada.Text_IO.File_Type;
   Start_Time : Ada.Calendar.Time;

   ---------------
   -- End_Trace --
   ---------------

   procedure End_Trace is
   begin
      Ada.Text_IO.Close (File);
   end End_Trace;

   -----------------
   -- Start_Trace --
   -----------------

   procedure Start_Trace (File_Name : String) is
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);
      Start_Time := Ada.Calendar.Clock;
   end Start_Trace;

   -----------
   -- Trace --
   -----------

   procedure Trace (Message : String) is
      use Ada.Calendar;
      Time_Stamp : constant Duration :=
                     Ada.Calendar.Clock - Start_Time;
   begin
      Ada.Text_IO.Put_Line
        (File,
         Duration'Image (Time_Stamp)
         & ": "
         & Ada.Task_Identification.Image
           (Ada.Task_Identification.Current_Task)
         & ": " & Message);
   end Trace;

end Marlowe.Trace;
