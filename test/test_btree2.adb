with Ada.Text_IO;
with Ada.Command_Line;

with WL.Trace;
with WL.Random;

with Marlowe.Keys;

procedure Test_Btree2 is

   function Image (X : Integer) return String is
   begin
      return Integer'Image (X);
   end Image;

   Min_Keys : constant := 7;

   package Keys is
      new Marlowe.Keys (Integer, "test", True, Min_Keys, Image);

   Size : constant := 10_000;
   Data : array (1 .. Size) of Integer;

begin

   WL.Trace.Start_Trace (3);
   WL.Random.Randomise;

   Keys.Create;

   for I in Data'Range loop
      Data (I) := I;
   end loop;

   for I in Data'Range loop
      declare
         Temp, Target : Integer;
      begin
         Target := WL.Random.Random_Number (1, Size);
         Temp := Data (Target);
         Data (Target) := Data (I);
         Data (I) := Temp;
      end;
   end loop;

   for I in Data'Range loop
      Btree_Operations.Insert (File, Data (I) / 100,
                               Marlowe.Database_Index (Data (I)));
   end loop;

   Keys.Close;

end Test_Btree2;
