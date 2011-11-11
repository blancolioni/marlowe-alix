with System.Storage_Elements;

with Marlowe.Page_Types;
with Marlowe.Pages.Table;

package Marlowe.Pages.Storage_Element_Page is
new Marlowe.Pages.Table (Marlowe.Page_Types.Data_Overflow_Page_Type,
                         System.Storage_Elements.Storage_Element);
