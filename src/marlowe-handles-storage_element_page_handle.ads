with System.Storage_Elements;

with Marlowe.Page_Types;
with Marlowe.Table_Page_Handles;
with Marlowe.Pages.Storage_Element_Page;

package Marlowe.Handles.Storage_Element_Page_Handle is
new Marlowe.Table_Page_Handles
  (Marlowe.Page_Types.Data_Overflow_Page_Type,
   System.Storage_Elements.Storage_Element, 0,
   Marlowe.Pages.Storage_Element_Page);
