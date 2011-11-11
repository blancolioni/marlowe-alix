with Ada.Text_IO;
with Ada.Command_Line;

with WL.Trace;
with WL.Random;

with Marlowe.Keys;

with Test_Keys;

procedure Test_Btree is

   package Keys renames Test_Keys;

--   package Keys is
--      new Marlowe.Keys (Integer, 7, Integer'Image);


--     procedure Dump_Tree (Index  : Marlowe.Btree.Page_Index;
--                          Indent : Natural)
--     is
--        Page      : Btree_Pages.Btree_Page;
--        Spaces : String (1 .. Indent) := (others => ' ');
--     begin
--        WL.Trace.Put_Line (Spaces & Marlowe.Btree.Images.Image (Index));
--        Ada.Text_IO.Put_Line (Spaces & Marlowe.Btree.Images.Image (Index));
--        Page := Btree_Pages.Get_Page (Index);

--        Btree_Pages.Dump_Page (Page);
--        if not Btree_Pages.Is_Leaf (Page) then
--           for I in 1 .. Btree_Pages.Number_Of_Keys (Page) loop
--              Dump_Tree (Btree_Pages.Get_Child (Page, I), Indent + 2);
--           end loop;
--        end if;
--     end Dump_Tree;

   Size : constant := 1_000_000;
   Data : array (1 .. Size) of Integer;
   Deleted : array (1 .. Size) of Boolean := (others => False);

   use Ada.Text_IO;
   use Marlowe;

begin

   WL.Trace.Start_Trace (3);

   if Ada.Command_Line.Argument_Count > 0 then
      Ada.Text_IO.Put_Line ("Creating tree ...");
      --  WL.Random.Randomise;


      Keys.Create ("test");

      for I in 1 .. Size loop
         Data (I) := I;
      end loop;

      for I in 1 .. Size loop
         declare
            Temp, Target : Integer;
         begin
            Target := WL.Random.Random_Number (1, Size);
            Temp := Data (Target);
            Data (Target) := Data (I);
            Data (I) := Temp;
         end;
      end loop;

      for I in 1 .. Size loop
         Keys.Insert (Data (I));
      end loop;

      Keys.Close;

   end if;

   Keys.Open ("test");

   Put_Line ("Checking btree structure");
   if not Keys.Tree_OK then
      Put_Line ("Tree_OK failed");
      Keys.Close;
      return;
   end if;

   Put_Line ("Quick search test ...");
   if not Keys.Exists (8) then
      Put_Line ("Could not find key 8");
   end if;

   Put_Line ("Exact search test ...");

   for I in 1 .. Size loop
      if not Keys.Exists (I) then
         Ada.Text_IO.Put_Line ("couldn't find" & I'Img);
         exit;
      end if;
   end loop;

   Put_Line ("Next test ...");
   --  Dump_Tree (Disk_Pages.Root_Page (File), 0);

   declare
      use Keys;
      use Marlowe;
      M : Mark := Search (Forward);
   begin
      for I in 1 .. Size loop
         if Valid (M) then
            if Get_Key (M) /= I then
               Put_Line ("Expected" & I'Img & "; found" &
                         Integer'Image (Get_Key (M)));
               exit;
            end if;
         else
            Put_Line ("Invalid mark at" & I'Img);
            exit;
         end if;
         Next (M);
      end loop;

      if Valid (M) then
         Put_Line ("Mark valid at end of loop with key" &
                   Integer'Image (Get_Key (M)));
      end if;
   end;

   Put_Line ("Inexact search test ...");
   --  Dump_Tree (Disk_Pages.Root_Page (File), 0);

   for I in 1 .. Size - 1 loop
      declare
         use Keys;
         M : Mark := Search (I, I + 1, Open, Closed, Forward);
      begin
         if not Valid (M) then
            Put_Line ("Open search for" & I'Img & " failed");
            exit;
         elsif Get_Key (M) /= I + 1 then
            Put_Line ("Open search for" & I'Img & " found" &
                      Integer'Image (Get_Key (M)));
            exit;
         end if;
      end;
   end loop;

   Put_Line ("Middle of two test ...");
   --  Dump_Tree (Disk_Pages.Root_Page (File), 0);

   for I in 2 .. Size - 1 loop
      declare
         use Keys;
         M : Mark := Search (I - 1, I + 1, Open, Open, Forward);
      begin
         if not Valid (M) then
            Put_Line ("Open search for" & I'Img & " failed");
            exit;
         elsif Get_Key (M) /= I then
            Put_Line ("Open search for" & I'Img & " found" &
                      Integer'Image (Get_Key (M)));
            exit;
         end if;
      end;
   end loop;

   Put_Line ("Backwards inexact search test ...");
   --  Dump_Tree (Disk_Pages.Root_Page (File), 0);

   for I in 1 .. Size - 1 loop
      declare
         use Keys;
         M : Mark :=
           Search (I, I + 1, Closed, Open, Backward);
      begin
         if not Valid (M) then
            Put_Line ("Open search for" & I'Img & " failed");
            exit;
         elsif Get_Key (M) /= I then
            Put_Line ("Open search for" & I'Img & " found" &
                      Integer'Image (Get_Key (M)));
            exit;
         end if;
      end;
   end loop;

   if Ada.Command_Line.Argument_Count > 1 then
      Put_Line ("Deleting random keys ...");
      for I in 1 .. Size / 5 loop
         declare
            D : Integer := WL.Random.Random_Number (1, Size);
         begin

            if Keys.Exists (D) then
               Keys.Delete (D);

--                 if not Keys.Tree_OK then
--                      Put_Line ("Tree not ok after deleting" & D'Img);
--                      Keys.Dump_Btree;
--                      exit;
--                 end if;

               --              Keys.Dump_Btree;
               --              if not Keys.Tree_OK then
               --                 Put_Line ("tree_ok failed at deletion" & I'Img &
               --                           " of key" & D'Img);
               --               Keys.Dump_Btree;
               --               exit;
               --            end if;
            end if;

            Deleted (D) := True;
         end;
      end loop;

      --     if Deleted (176) then
      --        if Keys.Exists (176) then
      --              Put_Line ("Deleted 176 but it still exists");
      --        end if;
      --     end if;

      for I in 1 .. Size loop
         if Deleted (I) then
            if Keys.Exists (I) then
               Put_Line ("Deleted" & I'Img & " but it still exists");
               exit;
            end if;
         elsif not Keys.Exists (I) then
            Put_Line ("Did not delete" & I'Img & " but it does not exist");
            exit;
         end if;
      end loop;

      if not Keys.Tree_OK then
         Put_Line ("Tree not ok after deletes");
      end if;
   end if;

   Keys.Dump_Btree;

   Put_Line ("Closing keys");

   Keys.Close;

end Test_Btree;
