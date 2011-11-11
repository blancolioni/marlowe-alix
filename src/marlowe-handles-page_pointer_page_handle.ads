with Marlowe.Page_Types;
with Marlowe.Table_Page_Handles;
with Marlowe.Pages.Page_Pointer_Page;

package Marlowe.Handles.Page_Pointer_Page_Handle is
new Marlowe.Table_Page_Handles
  (Marlowe.Page_Types.Page_Pointer_Page_Type,
   File_And_Page, Null_File_And_Page,
   Marlowe.Pages.Page_Pointer_Page);
