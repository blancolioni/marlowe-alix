with Marlowe.Page_Types;

package Marlowe.Pages.Data_Records is

   type Data_Record_Page_Record is private;
   type Data_Record_Page is access all Data_Record_Page_Record;

   function To_Data_Record_Page (Item : Page) return Data_Record_Page;
   function To_Page (Item : Data_Record_Page) return Page;

   procedure Initialise (Item          : in Data_Record_Page;
                         Location      : in File_And_Page;
                         Record_Size   : in Positive);

   procedure Release (Item : in out Data_Record_Page);

   function Get_Data_Record_Value (Item   : in Data_Record_Page;
                             Offset : Slot_Index)
                             return Contents_Type;

   procedure Set_Data_Record_Value (Item   : in Data_Record_Page;
                              Offset : Slot_Index;
                              Value  :  Contents_Type);


private

   subtype Data_Record_Entry_Index is
     Slot_Index range 1 .. Page_Contents_Bits / Contents_Type'Size;

   type Array_Of_Data_Record_Entries is
     array (Data_Record_Entry_Index) of Contents_Type;

   type Data_Record_Page_Record is
      record
         Header    : Page_Header;
         Contents  : Array_Of_Data_Record_Entries;
         Tail      : Page_Tail;
      end record;

   for Data_Record_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Data_Record;
