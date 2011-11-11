package Marlowe.Test_Db is

   procedure Create;
   procedure Open;
   procedure Close;

   type Database_Operation is access procedure;

   procedure Start_Insert_Task (Start, Finish : Integer);
   procedure Start_Search_Task (Start, Finish : Integer);

   procedure Wait;

end Marlowe.Test_Db;
