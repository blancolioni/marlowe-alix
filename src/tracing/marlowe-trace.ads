package Marlowe.Trace is

   pragma Elaborate_Body;

   procedure Start_Trace (File_Name : String);
   procedure End_Trace;

   procedure Put_Line (Message : String);

end Marlowe.Trace;
