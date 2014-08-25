package Marlowe.Trace is

   Tracing : Boolean := False;

   procedure Start_Trace (File_Name : String);
   procedure End_Trace;

   procedure Trace (Message : String);

end Marlowe.Trace;
