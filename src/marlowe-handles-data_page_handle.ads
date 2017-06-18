with Marlowe.Page_Types;
with Marlowe.Table_Page_Handles;
with Marlowe.Tables;

package Marlowe.Handles.Data_Page_Handle is
  new Marlowe.Table_Page_Handles
    (Table_Page_Type => Marlowe.Page_Types.Data_Page_Type,
     Header_Type     => Marlowe.Page_Types.No_Header,
     Contents_Type   => Marlowe.Tables.Record_Header);
