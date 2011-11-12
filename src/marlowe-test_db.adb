with Ada.Exceptions;
with Ada.Text_IO;

with System.Storage_Elements;

with Marlowe.Btree_Handles;
with Marlowe.Key_Storage;

--  with Marlowe.Btree_Keys;

package body Marlowe.Test_Db is

   Handle : Marlowe.Btree_Handles.Btree_Handle;
   Ref    : Marlowe.Btree_Handles.Btree_Reference;

   protected Wait_Object is
      procedure New_Task;
      procedure Task_Complete;
      entry Wait_For_Zero_Tasks;
   private
      Running : Natural := 0;
   end Wait_Object;

   protected body Wait_Object is

      procedure New_Task is
      begin
         Running := Running + 1;
      end New_Task;

      procedure Task_Complete is
      begin
         Running := Running - 1;
      end Task_Complete;

      entry Wait_For_Zero_Tasks when Running = 0 is
      begin
         null;
      end Wait_For_Zero_Tasks;

   end Wait_Object;

   task type Search_Task is
      entry Start (Low, High : Integer);
   end Search_Task;

   task type Insert_Task is
      entry Start (Low, High : Integer);
   end Insert_Task;

   -----------------
   -- Insert_Task --
   -----------------

   task body Insert_Task is
      First, Last : Integer;
   begin
      accept Start (Low : Integer; High : Integer) do
         First := Low;
         Last  := High;
      end Start;

      for I in First .. Last loop
         declare
            use type System.Storage_Elements.Storage_Array;
            use Marlowe.Btree_Handles;
            Index   : Database_Index;
            Item    : Integer := I;
         begin
            Index := Insert_Record (Handle, 1);
            Write_Record (Handle, 1, Index, Item'Address);

            Insert (Handle, Ref,
                    Marlowe.Key_Storage.To_Storage_Array (I, 4)
                    & Marlowe.Key_Storage.To_Storage_Array (Index));
         end;
      end loop;

      Wait_Object.Task_Complete;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Exception: " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Flush;
         Wait_Object.Task_Complete;
   end Insert_Task;

   task body Search_Task is
      First, Last : Integer;
   begin
      accept Start (Low : Integer; High : Integer) do
         First := Low;
         Last  := High;
      end Start;

      for I in First .. Last loop
         declare
            use type System.Storage_Elements.Storage_Array;
            use Marlowe.Btree_Handles;
            Index   : Database_Index;
            Low  : constant System.Storage_Elements.Storage_Array :=
                     Marlowe.Key_Storage.To_Storage_Array (I, 4)
                     & Marlowe.Key_Storage.To_Storage_Array
              (Database_Index'First);
            High  : constant System.Storage_Elements.Storage_Array :=
                     Marlowe.Key_Storage.To_Storage_Array (I, 4)
                     & Marlowe.Key_Storage.To_Storage_Array
              (Database_Index'Last);
            Mark  : constant Btree_Mark :=
                      Search (Handle, Ref, Low, High,
                              Open, Open, Forward);
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
                    Marlowe.Key_Storage.To_Database_Index
                      (Key (Key'First + 4 .. Key'Last));
                  Get_Record (Handle, 1, Index, Value'Address);
               end;

            end if;

         end;
      end loop;

      Wait_Object.Task_Complete;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Exception: " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Flush;
         Wait_Object.Task_Complete;
   end Search_Task;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Marlowe.Btree_Handles.Close (Handle);
   end Close;


   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Marlowe.Btree_Handles.Create (Handle, "test_btree.key", 0);
      declare
         Table : constant Marlowe.Table_Index :=
                   Marlowe.Btree_Handles.Add_Table (Handle, "test value", 4);
      begin

         Ref :=
           Marlowe.Btree_Handles.Add_Key
             (Handle, "test", Table, 12);
      end;
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Marlowe.Btree_Handles.Open (Handle, "test_btree.key", 0);
      Ref := Marlowe.Btree_Handles.Get_Reference (Handle, "test");
   end Open;

   -----------------------
   -- Start_Insert_Task --
   -----------------------

   procedure Start_Insert_Task (Start, Finish : Integer) is
      T : constant access Insert_Task := new Insert_Task;
   begin
      Wait_Object.New_Task;
      T.Start (Start, Finish);
   end Start_Insert_Task;

   -----------------------
   -- Start_Search_Task --
   -----------------------

   procedure Start_Search_Task (Start, Finish : Integer) is
      T : constant access Search_Task := new Search_Task;
   begin
      Wait_Object.New_Task;
      T.Start (Start, Finish);
   end Start_Search_Task;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Wait_Object.Wait_For_Zero_Tasks;
   end Wait;

end Marlowe.Test_Db;
