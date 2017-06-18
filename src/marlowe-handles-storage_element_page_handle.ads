with System.Storage_Elements;

with Marlowe.Tables;
with Marlowe.Page_Types;
with Marlowe.Table_Page_Handles;

package Marlowe.Handles.Storage_Element_Page_Handle is
  new Marlowe.Table_Page_Handles
    (Table_Page_Type => Marlowe.Page_Types.Storage_Element_Page_Type,
     Header_Type     => Marlowe.Page_Types.No_Header,
     Contents_Type   => System.Storage_Elements.Storage_Element);
