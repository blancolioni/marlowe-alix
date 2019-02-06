package Marlowe.Database is

   pragma Pure;

   type Database_Size_Range is new Database_Index;

   type Database_Information is
      record
         Size         : Database_Size_Range := 0;
         Blocks       : Natural := 0;
         Pages        : Natural := 0;
         Hits         : Natural := 0;
         Misses       : Natural := 0;
         Reads        : Natural := 0;
         Writes       : Natural := 0;
         New_Pages    : Natural := 0;
         Key_Count    : Natural := 0;
         Record_Count : Natural := 0;
      end record;

end Marlowe.Database;
