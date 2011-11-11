with Marlowe.Page_Types;
with Marlowe.Table_Page_Handles;
with Marlowe.Pages.Data_Page;
with Marlowe.Tables;

package Marlowe.Handles.Data_Page_Handle is
new Marlowe.Table_Page_Handles
  (Marlowe.Page_Types.Data_Page_Type,
   Marlowe.Tables.Record_Header,
   (Data => (others => 0), Overflow => Null_File_Page_And_Slot),
   Marlowe.Pages.Data_Page);
