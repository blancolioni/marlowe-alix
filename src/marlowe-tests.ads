with Marlowe.Btree_Handles;

package Marlowe.Tests is

   procedure Create (File_Name : String;
                     Key_Name  : String);

   procedure Insert (Handle        : Marlowe.Btree_Handles.Btree_Handle;
                     Ref           : Marlowe.Btree_Handles.Btree_Reference;
                     Start         : Integer;
                     Finish        : Integer;
                     Full_Checking : Boolean := False);

   procedure Check_Values (Handle   : Marlowe.Btree_Handles.Btree_Handle;
                           Ref      : Marlowe.Btree_Handles.Btree_Reference;
                           Start    : Integer;
                           Finish   : Integer);

end Marlowe.Tests;


