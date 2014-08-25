with Marlowe.Page_Types;

generic
   Table_Page_Type : Marlowe.Page_Types.Page_Type;
   type Contents_Type is private;
package Marlowe.Pages.Table is

   type Table_Page_Record is private;
   type Table_Page is access all Table_Page_Record;
   pragma No_Strict_Aliasing (Table_Page);

   function To_Table_Page (Item : Page) return Table_Page;
   function To_Page (Item : Table_Page) return Page;

   procedure Initialise (Item          : in Table_Page;
                         Location      : in File_And_Page;
                         Default_Value : in Contents_Type);

   procedure Release (Item : in out Table_Page);

   function Get_Table_Value (Item   : in Table_Page;
                             Offset : Slot_Index)
                             return Contents_Type;

   procedure Set_Table_Value (Item   : in Table_Page;
                              Offset : Slot_Index;
                              Value  :  Contents_Type);


private

   subtype Table_Entry_Index is
     Slot_Index range 1 .. Page_Contents_Bits / Contents_Type'Size;

   type Array_Of_Table_Entries is
     array (Table_Entry_Index) of Contents_Type;

   type Table_Page_Record is
      record
         Header    : Page_Header;
         Contents  : Array_Of_Table_Entries;
         Tail      : Page_Tail;
      end record;

   --   for Table_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Table;
