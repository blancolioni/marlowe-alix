with Ada.Text_IO;

with Marlowe.Btree_Keys;

package body Marlowe.Tests is

   ------------------
   -- Check_Values --
   ------------------

   procedure Check_Values (Handle   : Marlowe.Btree_Handles.Btree_Handle;
                           Ref      : Marlowe.Btree_Handles.Btree_Reference;
                           Start    : Integer;
                           Finish   : Integer)
   is
      use Marlowe.Btree_Handles;
      use Marlowe.Btree_Keys;
      Key_Def : constant Component_Array := Get_Key_Definition (Ref);
   begin

      Ada.Text_IO.Put_Line ("Existence check");
      for I in Start .. Finish loop
         if not Exists (Handle, Ref,
                        To_Btree_Key (Key_Def (Key_Def'First), I))
         then
            Ada.Text_IO.Put_Line ("Key" & I'Img & " did not exist");
         end if;
      end loop;

      if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
         Ada.Text_IO.Put_Line ("Tree not OK after existence check");
         return;
      end if;

   end Check_Values;

   ------------
   -- Create --
   ------------

   procedure Create (File_Name : String;
                     Key_Name  : String)
   is
      Handle : Marlowe.Btree_Handles.Btree_Handle;
   begin
      Marlowe.Btree_Handles.Create (Handle, File_Name, 0);
      declare
         Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
                 Marlowe.Btree_Handles.Add_Key
                   (Handle, Key_Name, 0,
                    (1 => Marlowe.Btree_Keys.Make_Component
                       (Marlowe.Btree_Keys.Signed_Integer_Key,
                        4)));
         pragma Unreferenced (Ref);
      begin
         null;
      end;

      Marlowe.Btree_Handles.Close (Handle);
   end Create;

   ------------
   -- Insert --
   ------------

   procedure Insert (Handle        : Marlowe.Btree_Handles.Btree_Handle;
                     Ref           : Marlowe.Btree_Handles.Btree_Reference;
                     Start         : Integer;
                     Finish        : Integer;
                     Full_Checking : Boolean := False)
   is
      use Marlowe.Btree_Handles;
      use Marlowe.Btree_Keys;
      Key_Def : constant Component_Array := Get_Key_Definition (Ref);
   begin

      Ada.Text_IO.Put_Line ("Inserting keys [" & Start'Img & " .." &
                            Finish'Img & " ]");
      for I in Start .. Finish loop
         --                 WL.Trace.Put_Line ("Inserting:" & I'Img);
         Insert (Handle, Ref, To_Btree_Key (Key_Def (Key_Def'First), I));

         if Full_Checking then
            if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
               Ada.Text_IO.Put_Line
                 ("Tree not OK after inserting item" & I'Img);
               return;
            end if;
         end if;
      end loop;

      if not Marlowe.Btree_Handles.Check_Tree (Handle, Ref) then
         Ada.Text_IO.Put_Line ("Tree not OK after insertions");
         return;
      end if;
   end Insert;

end Marlowe.Tests;


