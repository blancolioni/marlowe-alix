with Ada.Calendar;
with Ada.Task_Identification;
with Ada.Text_IO;

package body Marlowe.Trace is

   protected Mutex is
      entry Aquire;
      procedure Release;
   private
      Aquired : Boolean := False;
   end Mutex;

   Tracing : Boolean := False;
   File : Ada.Text_IO.File_Type;

   -----------
   -- Mutex --
   -----------

   protected body Mutex is
      entry Aquire when not Aquired is
      begin
         Aquired := True;
      end Aquire;

      procedure Release is
      begin
         Aquired := False;
      end Release;
   end Mutex;

--     protected Trace_File is
--        procedure Start (File_Name : String);
--        procedure Finish;
--        procedure Trace (Message : String);
--     private
--        Tracing : Boolean := False;
--        File : Ada.Text_IO.File_Type;
--     end Trace_File;

   ---------------
   -- End_Trace --
   ---------------

   procedure End_Trace is
   begin
      Tracing := False;
      Ada.Text_IO.Close (File);
   end End_Trace;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Message : String) is
      use Ada.Calendar;
      Now     : Time;
   begin
      if Tracing then
         Now := Clock;
         Mutex.Aquire;
         Ada.Text_IO.Put_Line (File, Day_Duration'Image (Seconds (Now)) &
                               ": " &
                               Ada.Task_Identification.Image
                               (Ada.Task_Identification.Current_Task) &
                               ": " & Message);
         Ada.Text_IO.Flush (File);
         Mutex.Release;
      end if;
   end Put_Line;

   -----------------
   -- Start_Trace --
   -----------------

   procedure Start_Trace (File_Name : String) is
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);
      Tracing := True;
   exception
      when others =>
         Tracing := False;
   end Start_Trace;

end Marlowe.Trace;
