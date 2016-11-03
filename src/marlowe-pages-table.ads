generic
   Table_Page_Type : Page_Type;
   type Header_Type is private;
   type Contents_Type is private;
package Marlowe.Pages.Table is

   type Table_Page_Record is private;
   type Table_Page is access all Table_Page_Record;
   pragma No_Strict_Aliasing (Table_Page);

   function To_Table_Page (Item : Page) return Table_Page;
   function To_Page (Item : Table_Page) return Page;

   procedure Initialise (Item          : in Table_Page;
                         Location      : in File_And_Page);

   procedure Release (Item : in out Table_Page);

   Page_Slot_Count : constant Slot_Index;

   function Last_Slot (Item : Table_Page) return Slot_Index;

   function Get_Header (Item : Table_Page) return Header_Type;
   procedure Set_Header (Item   : Table_Page;
                         Header : Header_Type);

   function Get_Table_Value (Item   : in Table_Page;
                             Offset : Slot_Index)
                             return Contents_Type;

   procedure Set_Table_Value (Item   : in Table_Page;
                              Offset : Slot_Index;
                              Value  :  Contents_Type);

private

   Last_Slot_Index : constant Slot_Index :=
                       (Page_Contents_Bits - Header_Type'Size)
                       / Contents_Type'Size;

   pragma Assert (Last_Slot_Index >= 1);

   subtype Table_Entry_Index is
     Slot_Index range 1 .. Last_Slot_Index;

   type Array_Of_Table_Entries is
     array (Table_Entry_Index) of Contents_Type;

   type Table_Page_Record is
      record
         Header       : Page_Header;
         Table_Header : Header_Type;
         Contents     : Array_Of_Table_Entries;
         Tail         : Page_Tail;
      end record;

   --   for Table_Page_Record'Size use Page_Bits;

   Page_Slot_Count : constant Slot_Index := Table_Entry_Index'Last;

   function Last_Slot (Item : Table_Page) return Slot_Index
   is (Page_Slot_Count);

   function Get_Header (Item : Table_Page) return Header_Type
   is (Item.Table_Header);

end Marlowe.Pages.Table;
