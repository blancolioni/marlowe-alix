with Marlowe.Btree_Handles;

package Marlowe.Test_Tasks is

   procedure Create_Tasks (Count : Positive);

   procedure Insert (Task_Index : Positive;
                     Handle     : Marlowe.Btree_Handles.Btree_Handle;
                     Ref        : Marlowe.Btree_Handles.Btree_Reference;
                     Start      : Integer;
                     Finish     : Integer);

   procedure Check_Values (Task_Index : Positive;
                           Handle     : Marlowe.Btree_Handles.Btree_Handle;
                           Ref        : Marlowe.Btree_Handles.Btree_Reference;
                           Start      : Integer;
                           Finish     : Integer);

   procedure Wait;

end Marlowe.Test_Tasks;
