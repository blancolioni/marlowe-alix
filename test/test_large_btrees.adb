with Ada.Text_IO;                       use Ada.Text_IO;

with WL.Trace;
with WL.Random;

with Marlowe.Btree_Handles;
with Marlowe.Btree_Keys;

procedure Test_Large_Btrees is
   Handle : Marlowe.Btree_Handles.Btree_Handle;
   Ref    : Marlowe.Btree_Handles.Btree_Reference;
begin

   WL.Trace.Start_Trace (3);

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

   Marlowe.Btree_Handles.Open (Handle, "test_btree.key");

   declare
      use Marlowe.Btree_Handles;
      use Marlowe.Btree_Keys;
      Ref : constant Btree_Reference := Get_Reference (Handle, "test");
      Key_Def : constant Component_Array := Get_Key_Definition (Ref);
      --  Size : constant := 16467327;
      Size : constant := 100_000_000;
   begin

      Ada.Text_IO.Put_Line ("Inserting keys");
      for I in 1 .. Size loop
         begin
            Insert (Handle, Ref, To_Btree_Key (Key_Def (Key_Def'First), I));
            if I mod 100_000 = 0 then
               Ada.Text_IO.Put_Line (I'Img);
            end if;
--              if I > 16_467_000 then
--                 if not Check_Tree (Handle, Ref) then
--                    Ada.Text_IO.Put_Line ("Tree not OK after insertion of" &
--                                          I'Img);
--                    Marlowe.Btree_Handles.Close (Handle);
--                    return;
--                 end if;
--              end if;
         exception
            when others =>
               Ada.Text_IO.Put_Line ("Exception raised; I =" & I'Img);
               raise;
         end;
      end loop;

      Ada.Text_IO.Put_Line ("Existence check");
      for I in 1 .. Size loop
         if not Exists (Handle, Ref,
                        To_Btree_Key (Key_Def (Key_Def'First), I))
         then
            Ada.Text_IO.Put_Line ("Key" & I'Img & " did not exist");
         end if;
      end loop;

--        Ada.Text_IO.Put_Line ("Deleting random keys ...");
--        for I in 1 .. Size / 5 loop
--           declare
--              D : Integer := WL.Random.Random_Number (1, Size);
--           begin

--              if Exists (Handle, Ref,
--                         To_Btree_Key (Key_Def (Key_Def'First), D))
--              then
--                 Delete (Handle, Ref,
--                         To_Btree_Key (Key_Def (Key_Def'First), D));
--              end if;
--           end;
--        end loop;

      if not Check_Tree (Handle, Ref) then
         Ada.Text_IO.Put_Line ("Tree not OK after tests");
      end if;

   end;


   Ada.Text_IO.Put_Line ("Closing btree");
   Marlowe.Btree_Handles.Close (Handle);

--     declare
--        use Marlowe.Btree_Keys;
--        Test_Comp : constant Key_Component := Make_Component (String_Key, 16);
--        Test_Key  : constant Component_Array := (1 => Test_Comp);
--     begin
--        for I in 7 .. 13 loop
--           declare
--              Left  : constant System.Storage_Elements.Storage_Array :=
--                To_Btree_Key (Test_Comp, Integer'Image (I));
--              Right : constant System.Storage_Elements.Storage_Array :=
--                To_Btree_Key (Test_Comp, Integer'Image (I + 2));
--           begin
--              Put_Line ("Compare (" & Integer'Image (I) & "," &
--                        Integer'Image (I + 2) & ") = " &
--                        Compare_Result'Image (Compare (Test_Key, Left, Right)));
--           end;
--        end loop;
--     end;

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

end Test_Large_Btrees;
