with System.Storage_Elements;

with Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

with WL.Random;

with Marlowe.Btree_Handles;
with Marlowe.Btree_Keys;

with Marlowe.Allocation;
with Marlowe.Debug;

with Marlowe.Test_Tasks;

with Marlowe.Trace;

procedure Marlowe.Driver is

   Default_Size : constant := 10_000;

   File_Name : constant String := "test_btree.key";
   Key_Name  : constant String := "test";

   type Key_Data is array (Positive range <>) of Natural;
   type Key_Data_Access is access Key_Data;

   type Key_Flag is array (Positive range <>) of Boolean;
   type Key_Flag_Access is access Key_Flag;

   Size : Integer := Default_Size;

   Do_Create          : Boolean := True;
   Do_Insert          : Boolean := True;
   Do_Existence_Check : Boolean := True;
   Do_Delete          : Boolean := True;
   Random_Keys        : Boolean := True;
   Full_Checking      : Boolean := False;
   Num_Tasks          : Natural := 0;
   Set_Size           : Boolean := False;
   Set_Num_Tasks      : Boolean := False;
   Set_Debug          : Boolean := False;
   Debugging          : constant Boolean := False;

   Start_Time         : Ada.Calendar.Time;
   Insert_Finish      : Ada.Calendar.Time;
   --  Existence_Finish   : Ada.Calendar.Time;
   --  Delete_Finish      : Ada.Calendar.Time;
   --  Check_Finish       : Ada.Calendar.Time;

   Trace_File         : Ada.Text_IO.File_Type;

begin

   for I in 1 .. Argument_Count loop
      if Set_Size then
         begin
            Size := Positive'Value (Argument (I));
            Set_Size := False;
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put_Line ("Bad size argument");
               return;
         end;
      elsif Set_Num_Tasks then
         begin
            Num_Tasks := Positive'Value (Argument (I));
            Set_Num_Tasks := False;
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put_Line ("Bad task count argument");
               return;
         end;
      elsif Set_Debug then
         --  Debugging := True;
         Marlowe.Debug.Enable (Argument (I));
         Set_Debug := False;
      elsif Argument (I) = "--no-create" or Argument (I) = "-c" then
         Do_Create := False;
      elsif Argument (I) = "--no-insert" or Argument (I) = "-i" then
         Do_Insert := False;
      elsif Argument (I) = "--no-existence-check" or Argument (I) = "-e" then
         Do_Existence_Check := False;
      elsif Argument (I) = "--no-delete" or Argument (I) = "-d" then
         Do_Delete := False;
      elsif Argument (I) = "--no-random" or Argument (I) = "-r" then
         Random_Keys := False;
      elsif Argument (I) = "--full-checking" or Argument (I) = "-f" then
         Full_Checking := True;
      elsif Argument (I) = "--size" or Argument (I) = "-s" then
         Set_Size := True;
      elsif Argument (I) = "--tasks" or Argument (I) = "-t" then
         Set_Num_Tasks := True;
      elsif Argument (I) = "--debug" or Argument (I) = "-D" then
         Set_Debug := True;
      else
         Ada.Text_IO.Put_Line ("Unknown argument: " & Argument (I));
         return;
      end if;
   end loop;

   if Set_Size then
      Ada.Text_IO.Put_Line ("missing size");
      return;
   end if;

   if Debugging then
      Create (Trace_File, Ada.Text_IO.Out_File, "trace.txt");
      Ada.Text_IO.Set_Output (Trace_File);
      Marlowe.Trace.Tracing := True;
   end if;

   if Do_Create then
      declare
         Handle : Marlowe.Btree_Handles.Btree_Handle;
      begin
         Marlowe.Btree_Handles.Create (Handle, "test_btree.key", 0);
         declare
            Table : constant Marlowe.Table_Index :=
              Marlowe.Btree_Handles.Add_Table (Handle, "test value", 4);
            Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Marlowe.Btree_Handles.Add_Key
              (Handle, "test", Table,
               (1 => Marlowe.Btree_Keys.Make_Component
                (Marlowe.Btree_Keys.Signed_Integer_Key,
                   4),
                2 => Marlowe.Btree_Keys.Make_Component
                  (Marlowe.Btree_Keys.Unsigned_Integer_Key, 8)));
            pragma Unreferenced (Ref);
         begin
            null;
         end;

         Marlowe.Btree_Handles.Close (Handle);
      end;
   end if;

   if Num_Tasks > 0 then
      declare
         Handle  : Marlowe.Btree_Handles.Btree_Handle;
      begin
         Marlowe.Btree_Handles.Open (Handle, File_Name, 0);
         Marlowe.Test_Tasks.Create_Tasks (Num_Tasks);
         declare
            Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Marlowe.Btree_Handles.Get_Reference (Handle, Key_Name);
         begin
            for I in 1 .. Num_Tasks loop
               Marlowe.Test_Tasks.Insert (I, Handle, Ref,
                                          Size * (I - 1) / Num_Tasks + 1,
                                          Size * I / Num_Tasks);
            end loop;
            Marlowe.Test_Tasks.Wait;
            for I in 1 .. Num_Tasks loop
               Marlowe.Test_Tasks.Check_Values (I, Handle, Ref,
                                                Size * (I - 1) / Num_Tasks + 1,
                                                Size * I / Num_Tasks);
            end loop;
            Marlowe.Test_Tasks.Wait;
         end;
         Marlowe.Btree_Handles.Close (Handle);
         Marlowe.Allocation.Report;
      end;
      return;
   end if;

   declare
      use type System.Storage_Elements.Storage_Array;
      Data    : constant Key_Data_Access := new Key_Data (1 .. Size);
      Deleted : constant Key_Flag_Access := new Key_Flag'(1 .. Size => False);
   begin

      for I in 1 .. Size loop
         Data (I) := I;
      end loop;

      if Random_Keys then
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
      end if;

      if Do_Insert then
         declare
            Handle  : Marlowe.Btree_Handles.Btree_Handle;
         begin
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key", 0);

            declare
               use type Ada.Calendar.Time;
               use Marlowe.Btree_Handles;
               use Marlowe.Btree_Keys;
               Ref : constant Btree_Reference :=
                       Get_Reference (Handle, "test");
               Key_Def : constant Component_Array := Get_Key_Definition (Ref);
               Index   : Database_Index;
               Item    : Integer;
            begin

               Ada.Text_IO.Put_Line ("Inserting" & Size'Img & " keys");

               Start_Time := Ada.Calendar.Clock;

               for I in 1 .. Size loop
                  --                 WL.Trace.Put_Line ("Inserting:" & I'Img);
                  Item := Data (I);
                  Index := Insert_Record (Handle, 1);
                  Write_Record (Handle, 1, Index, Item'Address);

                  Insert (Handle, Ref,
                          To_Btree_Key (Key_Def (Key_Def'First), Data (I)) &
                          To_Btree_Key (Index));

                  if Full_Checking then
                     if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
                        Ada.Text_IO.Put_Line
                          ("Tree not OK after inserting item" & I'Img &
                           " with value" & Integer'Image (Data (I)));
                        Marlowe.Btree_Handles.Close (Handle);
                        return;
                     end if;
                  end if;
               end loop;

               Insert_Finish := Ada.Calendar.Clock;

               Ada.Text_IO.Put_Line
                 ("Time:" &
                  Duration'Image (Insert_Finish - Start_Time) & "s");

               if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
                  Ada.Text_IO.Put_Line ("Tree not OK after insertions");
                  Marlowe.Btree_Handles.Close (Handle);
                  return;
               end if;
            end;

            Marlowe.Btree_Handles.Close (Handle);

         end;

      end if;

      if Do_Existence_Check then
         declare
            Handle  : Marlowe.Btree_Handles.Btree_Handle;
         begin
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key", 0);

            declare
               use Marlowe.Btree_Handles;
               use Marlowe.Btree_Keys;
               Ref : constant Btree_Reference :=
                 Get_Reference (Handle, "test");
               Key_Def : constant Component_Array := Get_Key_Definition (Ref);
            begin

               Ada.Text_IO.Put_Line ("Existence check");
               for I in 1 .. Size loop
                  declare
                     Low  : constant System.Storage_Elements.Storage_Array :=
                              To_Btree_Key (Key_Def (Key_Def'First), I) &
                     To_Btree_Key (Database_Index'First);
                     High  : constant System.Storage_Elements.Storage_Array :=
                              To_Btree_Key (Key_Def (Key_Def'First), I) &
                     To_Btree_Key (Database_Index'Last);
                     Mark  : constant Btree_Mark :=
                               Search (Handle, Ref, Low, High,
                                       Open, Open, Forward);
                     Index : Database_Index;
                     Value : Integer;
                  begin
                     if not Valid (Mark) then
                        Ada.Text_IO.Put_Line ("Key" & I'Img &
                                              " did not exist");
                     else
                        declare
                           use System.Storage_Elements;
                           Key : constant Storage_Array := Get_Key (Mark);
                        begin
                           Index :=
                             To_Database_Index
                               (Key (Key'First + 4 .. Key'Last));
                           Get_Record (Handle, 1, Index, Value'Address);
                        end;

                     end if;


                  end;
               end loop;

               if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
                  Ada.Text_IO.Put_Line ("Tree not OK after existence check");
                  Marlowe.Btree_Handles.Close (Handle);
                  return;
               end if;
            end;

            Marlowe.Btree_Handles.Close (Handle);

         end;

      end if;

      if Do_Delete then
         declare
            Handle  : Marlowe.Btree_Handles.Btree_Handle;
         begin
            --  Marlowe.Trace.Tracing := True;
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key", 0);

            declare
               use Marlowe.Btree_Handles;
               use Marlowe.Btree_Keys;
               Ref : constant Btree_Reference :=
                 Get_Reference (Handle, "test");
               Key_Def : constant Component_Array := Get_Key_Definition (Ref);
            begin

               Ada.Text_IO.Put_Line ("Deleting random keys ...");
               for I in 1 .. Size / 5 loop
                  declare
                     D : constant Integer := WL.Random.Random_Number (1, Size);
                     Low  : constant System.Storage_Elements.Storage_Array :=
                              To_Btree_Key (Key_Def (Key_Def'First), D) &
                       To_Btree_Key (Database_Index'First);
                     High  : constant System.Storage_Elements.Storage_Array :=
                              To_Btree_Key (Key_Def (Key_Def'First), D) &
                       To_Btree_Key (Database_Index'Last);
                     Key   : System.Storage_Elements.Storage_Array := Low;
                     Found : Boolean := True;
                  begin
                     declare
                        Mark  : constant Btree_Mark :=
                          Search (Handle, Ref, Low, High,
                                  Open, Open, Forward);
                     begin
                        if Valid (Mark) then
                           Key := Get_Key (Mark);
                        else
                           Found := False;
                           if not Deleted (D) then
                              Ada.Text_IO.Put_Line
                                ("should have found key:" & D'Img);
                           end if;
                        end if;
                     end;

                     if Found then
                        --  Ada.Text_IO.Put_Line ("Deleting:" & D'Img);
                        Delete (Handle, Ref, Key);
                        Deleted (D) := True;
                     end if;
                  end;
               end loop;

               if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
                  Ada.Text_IO.Put_Line ("Tree not OK after deletions");
                  Marlowe.Btree_Handles.Close (Handle);
                  return;
               end if;
            end;

            Marlowe.Btree_Handles.Close (Handle);

         end;

         declare
            Handle  : Marlowe.Btree_Handles.Btree_Handle;
         begin
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key", 0);

            declare
               use Marlowe.Btree_Handles;
               use Marlowe.Btree_Keys;
               Ref : constant Btree_Reference :=
                 Get_Reference (Handle, "test");
               Key_Def : constant Component_Array := Get_Key_Definition (Ref);
            begin
               Ada.Text_IO.Put_Line ("Checking deletions");

               for I in 1 .. Size loop
                  declare
                     Low  : constant
                       System.Storage_Elements.Storage_Array :=
                       To_Btree_Key (Key_Def (Key_Def'First), I) &
                       To_Btree_Key (Database_Index'First);
                     High  : constant
                       System.Storage_Elements.Storage_Array :=
                       To_Btree_Key (Key_Def (Key_Def'First), I) &
                       To_Btree_Key (Database_Index'Last);
                     Mark  : constant Btree_Mark :=
                       Search (Handle, Ref, Low, High,
                               Open, Open, Forward);
                  begin
                     if Deleted (I) then
                        if Valid (Mark) then
                           Ada.Text_IO.Put_Line ("Deleted" & I'Img &
                                                   " but it still exists");
                        end if;
                     elsif not Valid (Mark) then
                        Ada.Text_IO.Put_Line ("Did not delete" & I'Img &
                                                " but it does not exist");
                     end if;
                  end;
               end loop;

               if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
                  Ada.Text_IO.Put_Line ("Tree not OK after deletion check");
                  Marlowe.Btree_Handles.Close (Handle);
                  return;
               end if;
            end;

            Marlowe.Btree_Handles.Close (Handle);

         end;
      end if;

   end;


   declare
      use Marlowe.Btree_Keys;
      Test_Component : constant Key_Component :=
        Make_Component (Signed_Integer_Key, 4);
      Test_Key : constant Component_Array :=
        (1 => Test_Component);
   begin
      for I in -50 .. 50 loop
         for J in -50 .. 50 loop
            case Compare (Test_Key,
                          To_Btree_Key (Test_Component, I),
                          To_Btree_Key (Test_Component, J))
            is
               when Less =>
                  if I >= J then
                     Put_Line (I'Img & " compared less than" & J'Img);
                  end if;
               when Equal =>
                  if I /= J then
                     Put_Line (I'Img & " compared equal to " & J'Img);
                  end if;
               when Greater =>
                  if I <= J then
                     Put_Line (I'Img & " compared greater than" & J'Img);
                  end if;
            end case;
         end loop;
      end loop;
   end;

   Marlowe.Allocation.Report;

   if Debugging then
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (Trace_File);
   end if;

end Marlowe.Driver;
