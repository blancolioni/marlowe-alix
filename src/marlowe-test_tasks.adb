with Marlowe.Tests;

package body Marlowe.Test_Tasks is

   task type Tester is
      entry Insert (Handle     : Marlowe.Btree_Handles.Btree_Handle;
                    Ref        : Marlowe.Btree_Handles.Btree_Reference;
                    Start      : Integer;
                    Finish     : Integer);
      entry Check_Values (Handle     : Marlowe.Btree_Handles.Btree_Handle;
                          Ref        : Marlowe.Btree_Handles.Btree_Reference;
                          Start      : Integer;
                          Finish     : Integer);
      entry Finish;
   end Tester;

   type Tester_Access is access Tester;

   Max_Tasks  : constant := 100;
   Task_List  : array (1 .. Max_Tasks) of Tester_Access;
   Task_Count : Natural;

   ------------------
   -- Check_Values --
   ------------------

   procedure Check_Values (Task_Index : Positive;
                           Handle     : Marlowe.Btree_Handles.Btree_Handle;
                           Ref        : Marlowe.Btree_Handles.Btree_Reference;
                           Start      : Integer;
                           Finish     : Integer)
   is
   begin
      Task_List (Task_Index).Check_Values (Handle, Ref, Start, Finish);
   end Check_Values;

   ------------------
   -- Create_Tasks --
   ------------------

   procedure Create_Tasks (Count : Positive) is
   begin
      Task_Count := Count;
      for I in 1 .. Task_Count loop
         Task_List (I) := new Tester;
      end loop;
   end Create_Tasks;

   ------------
   -- Insert --
   ------------

   procedure Insert (Task_Index : Positive;
                     Handle     : Marlowe.Btree_Handles.Btree_Handle;
                     Ref        : Marlowe.Btree_Handles.Btree_Reference;
                     Start      : Integer;
                     Finish     : Integer)
   is
   begin
      Task_List (Task_Index).Insert (Handle, Ref, Start, Finish);
   end Insert;


   ------------
   -- Tester --
   ------------

   task body Tester is
      H                   : Marlowe.Btree_Handles.Btree_Handle;
      My_Ref              : Marlowe.Btree_Handles.Btree_Reference;
      My_Start, My_Finish : Integer;
   begin
      loop
         select
            accept Insert (Handle     : Marlowe.Btree_Handles.Btree_Handle;
                           Ref        : Marlowe.Btree_Handles.Btree_Reference;
                           Start      : Integer;
                           Finish     : Integer)
            do
               H := Handle;
               My_Ref      := Ref;
               My_Start    := Start;
               My_Finish   := Finish;
            end Insert;
            Marlowe.Tests.Insert (H, My_Ref, My_Start, My_Finish);
            accept Finish;
         or
            accept Check_Values
              (Handle     : Marlowe.Btree_Handles.Btree_Handle;
               Ref        : Marlowe.Btree_Handles.Btree_Reference;
               Start      : Integer;
               Finish     : Integer)
            do
               H := Handle;
               My_Ref      := Ref;
               My_Start    := Start;
               My_Finish   := Finish;
            end Check_Values;
            Marlowe.Tests.Check_Values (H, My_Ref, My_Start, My_Finish);
            accept Finish;
         or
            terminate;
         end select;
      end loop;
   end Tester;

   procedure Wait is
   begin
      for I in 1 .. Task_Count loop
         Task_List (I).Finish;
      end loop;
   end Wait;

end Marlowe.Test_Tasks;
