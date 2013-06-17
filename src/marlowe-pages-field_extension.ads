package Marlowe.Pages.Field_Extension is

   type Field_Extension_Page_Record is private;
   type Field_Extension_Page is access all Field_Extension_Page_Record;

   function To_Field_Extension_Page (Item : Page) return Field_Extension_Page;
   function To_Page (Item : Field_Extension_Page) return Page;

   procedure Initialise (Item          : in Field_Extension_Page;
                         Location      : in File_And_Page);

   procedure Release (Item : in out Field_Extension_Page);

   function Field_Extension_Length
     (Item : Field_Extension_Page)
      return System.Storage_Elements.Storage_Count;

   function Overflow_Page
     (Item : Field_Extension_Page)
      return File_And_Page;

   procedure Set_Overflow_Page
     (Item     : Field_Extension_Page;
      Overflow : File_And_Page);

   procedure Get_Contents
     (Item : Field_Extension_Page;
      Data : out System.Storage_Elements.Storage_Array);

   procedure Set_Contents
     (Item : Field_Extension_Page;
      Data : System.Storage_Elements.Storage_Array;
      Next : out System.Storage_Elements.Storage_Offset);
   --  Copy Data to the page, up to the maximum length.
   --  Next is set to the index of the first unwritten
   --  storage element, or Data'Last + 1 if everything was written.

private

   Max_Extension_Data : constant :=
                          (Page_Contents_Bits - File_And_Page_Bits - 16)
                          / System.Storage_Unit;

   subtype Extension_Data is
     System.Storage_Elements.Storage_Array
       (1 .. Max_Extension_Data);

   type Field_Extension_Page_Contents is
      record
         Overflow      : File_And_Page;
         Length        : Slot_Index;
         Data          : Extension_Data;
      end record;

   for Field_Extension_Page_Contents'Size use Page_Contents_Bits;

   type Field_Extension_Page_Record is
      record
         Header    : Page_Header;
         Contents  : Field_Extension_Page_Contents;
         Tail      : Page_Tail;
      end record;

   for Field_Extension_Page_Record'Size use Page_Bits;

   --   for Field_Extension_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Field_Extension;
