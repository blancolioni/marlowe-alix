with Marlowe.Page_Types;
with Marlowe.Pages.Table;

package Marlowe.Pages.Direct_Data_Pointer_Page is
new Marlowe.Pages.Table (Marlowe.Page_Types.Direct_Data_Pointer_Page_Type,
                         File_Page_And_Slot);
