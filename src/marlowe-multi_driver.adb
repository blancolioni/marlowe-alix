with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

with Marlowe.Test_Db;
with Marlowe.Trace;

procedure Marlowe.Multi_Driver is

   Create_Database      : constant Boolean := True;
   Search_Database      : constant Boolean := True;

   Dataset_Size         : constant := 100_000;
   Insert_Task_Count    : constant := 1;
   Search_Task_Count    : constant := 2;
   Insert_Per_Task_Size : constant := Dataset_Size / Insert_Task_Count;
   Search_Per_Task_Size : constant := Dataset_Size / Search_Task_Count;

   Trace_File           : Ada.Text_IO.File_Type;

   Start_Time         : Ada.Calendar.Time;
   Finish_Time        : Ada.Calendar.Time;

begin

--     Ada.Text_IO.Create (Trace_File, Ada.Text_IO.Out_File, "trace.txt");
--     Ada.Text_IO.Set_Output (Trace_File);

   if Create_Database then
      Marlowe.Test_Db.Create;

      Start_Time := Ada.Calendar.Clock;

      for I in 1 .. Insert_Task_Count loop
         declare
            Start  : constant Positive :=
                       (I - 1) * Insert_Per_Task_Size + 1;
            Finish : constant Positive :=
                       Start + Insert_Per_Task_Size - 1;
         begin
            Ada.Text_IO.Put_Line ("Starting task" & I'Img);
            Marlowe.Test_Db.Start_Insert_Task (Start, Finish);
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Waiting for" & Insert_Task_Count'Img
                              & " tasks to finish");
      Marlowe.Test_Db.Wait;

      Finish_Time := Ada.Calendar.Clock;

      declare
         use type Ada.Calendar.Time;
         D : constant Duration := Finish_Time - Start_Time;
      begin
         Ada.Text_IO.Put_Line ("Inserting: Task count =" &
                               Positive'Image (Insert_Task_Count) &
                               "; # records =" &
                               Positive'Image (Dataset_Size) &
                                 "; time =" & D'Img & "s");
      end;

      Marlowe.Test_Db.Close;

      Ada.Text_IO.Put_Line ("database closed");
   end if;

   if Search_Database then

      Marlowe.Trace.Tracing := False;

      Marlowe.Test_Db.Open;

      Start_Time := Ada.Calendar.Clock;

      for I in 1 .. Search_Task_Count loop
         declare
            Start  : constant Positive :=
                       (I - 1) * Search_Per_Task_Size + 1;
            Finish : constant Positive :=
                       Start + Search_Per_Task_Size - 1;
         begin
            Ada.Text_IO.Put_Line ("Starting task" & I'Img);
            Marlowe.Test_Db.Start_Search_Task (Start, Finish);
         end;
      end loop;

      Ada.Text_IO.Put_Line ("Waiting for" & Search_Task_Count'Img
                              & " tasks to finish");
      Marlowe.Test_Db.Wait;

      Finish_Time := Ada.Calendar.Clock;

      declare
         use type Ada.Calendar.Time;
         D : constant Duration := Finish_Time - Start_Time;
      begin
         Ada.Text_IO.Put_Line ("Searching: Task count =" &
                               Positive'Image (Search_Task_Count) &
                               "; # records =" &
                               Positive'Image (Dataset_Size) &
                               "; time =" & D'Img & "s");
      end;

      Marlowe.Test_Db.Close;
   end if;

--     Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
--     Ada.Text_IO.Close (Trace_File);

exception

   when E : others =>
      Ada.Text_IO.Put_Line ("Exception: " &
                               Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Flush;
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (Trace_File);

end Marlowe.Multi_Driver;
