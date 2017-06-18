with Marlowe.Page_Types;
with Marlowe.Table_Page_Handles;

package Marlowe.Handles.Page_Pointer_Page_Handle is
  new Marlowe.Table_Page_Handles
    (Table_Page_Type => Marlowe.Page_Types.Page_Pointer_Page_Type,
     Header_Type     => Marlowe.Page_Types.No_Header,
     Contents_Type   => File_And_Page);
