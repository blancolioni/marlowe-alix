with Marlowe.Page_Types;
with Marlowe.Table_Page_Handles;
with Marlowe.Pages.Direct_Data_Pointer_Page;

package Marlowe.Handles.Direct_Data_Pointer_Page_Handle is
new Marlowe.Table_Page_Handles
  (Marlowe.Page_Types.Direct_Data_Pointer_Page_Type,
   File_Page_And_Slot,
   Marlowe.Pages.Direct_Data_Pointer_Page);
