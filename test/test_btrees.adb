with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

with WL.Trace;
with WL.Random;

with Marlowe.Btree_Handles;
with Marlowe.Btree_Keys;

procedure Test_Btrees is

   Default_Size : constant := 10_000_000;

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

   Set_Size           : Boolean := False;

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
      else
         Ada.Text_IO.Put_Line ("Unknown argument: " & Argument (I));
         return;
      end if;
   end loop;

   if Set_Size then
      Ada.Text_IO.Put_Line ("missing size");
      return;
   end if;

   WL.Trace.Start_Trace (3);

   if Do_Create then
      declare
         Handle : Marlowe.Btree_Handles.Btree_Handle;
      begin
         Marlowe.Btree_Handles.Create (Handle, "test_btree.key");
         declare
            Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
              Marlowe.Btree_Handles.Add_Key
              (Handle, "test",
               (1 => Marlowe.Btree_Keys.Make_Component
                (Marlowe.Btree_Keys.Signed_Integer_Key,
                 4)));
         begin
            null;
         end;

         Marlowe.Btree_Handles.Close (Handle);
      end;
   end if;

   declare
      Data    : Key_Data_Access := new Key_Data (1 .. Size);
      Deleted : Key_Flag_Access := new Key_Flag'(1 .. Size => False);
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
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key");

            declare
               use Marlowe.Btree_Handles;
               use Marlowe.Btree_Keys;
               Ref : constant Btree_Reference := Get_Reference (Handle, "test");
               Key_Def : constant Component_Array := Get_Key_Definition (Ref);
            begin

               Ada.Text_IO.Put_Line ("Inserting keys");
               for I in 1 .. Size loop
                  --                 WL.Trace.Put_Line ("Inserting:" & I'Img);
                  Insert (Handle, Ref, To_Btree_Key (Key_Def (Key_Def'First),
                                                     Data (I)));

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
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key");

            declare
               use Marlowe.Btree_Handles;
               use Marlowe.Btree_Keys;
               Ref : constant Btree_Reference :=
                 Get_Reference (Handle, "test");
               Key_Def : constant Component_Array := Get_Key_Definition (Ref);
            begin

               Ada.Text_IO.Put_Line ("Existence check");
               for I in 1 .. Size loop
                  if not Exists (Handle, Ref,
                                 To_Btree_Key (Key_Def (Key_Def'First), I))
                  then
                     Ada.Text_IO.Put_Line ("Key" & I'Img & " did not exist");
                  end if;
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
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key");

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
                     D : Integer := WL.Random.Random_Number (1, Size);
                  begin

                     if Exists (Handle, Ref,
                                To_Btree_Key (Key_Def (Key_Def'First), D))
                     then
                        Delete (Handle, Ref,
                                To_Btree_Key (Key_Def (Key_Def'First), D));
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
            Marlowe.Btree_Handles.Open (Handle, "test_btree.key");

            declare
               use Marlowe.Btree_Handles;
               use Marlowe.Btree_Keys;
               Ref : constant Btree_Reference :=
                 Get_Reference (Handle, "test");
               Key_Def : constant Component_Array := Get_Key_Definition (Ref);
            begin
               Ada.Text_IO.Put_Line ("Checking deletions");

               for I in 1 .. Size loop
                  if Deleted (I) then
                     if Exists (Handle, Ref,
                                To_Btree_Key (Key_Def (Key_Def'First), I))
                     then
                        Ada.Text_IO.Put_Line ("Deleted" & I'Img &
                                              " but it still exists");
                        exit;
                     end if;
                  elsif not Exists (Handle, Ref,
                                    To_Btree_Key (Key_Def (Key_Def'First), I))
                  then
                     Ada.Text_IO.Put_Line ("Did not delete" & I'Img &
                                           " but it does not exist");
                     exit;
                  end if;
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

end Test_Btrees;
