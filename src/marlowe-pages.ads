with System.Storage_Elements;

with Marlowe.Sizes;

package Marlowe.Pages is

   Page_Size_Bits    : constant := 12;
   Page_Size         : constant := 2**Page_Size_Bits;
   Max_Pages_In_File : constant := 2**(31 - Page_Size_Bits);

   type Page_Record is private;

   type Page is access all Page_Record;
   pragma No_Strict_Aliasing (Page);

   function Get_File_Index (From : Page) return File_Index;
   function Get_Page_Index (From : Page) return Page_Index;
   function Get_File_And_Page (From : Page) return File_And_Page;

   function Get_File_Index (From : Page_Record) return File_Index;
   function Get_Page_Index (From : Page_Record) return Page_Index;
   function Get_File_And_Page (From : Page_Record) return File_And_Page;

   function Get_Page_Type (From : Page)
                          return Page_Type;

   procedure Release (Item : in out Page);

   function Validate (Item : Page_Record) return Boolean;

   procedure Initialise (Item     : in Page;
                         Identity : in Page_Type;
                         Location : in File_And_Page);

   function Elements_Per_Page
     (Element_Size : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count;

private

   Header_Magic : constant := 16#79a75ecd#;
   Tail_Magic   : constant := 16#81646b28#;

   type Word_4 is mod 2**32;
   for Word_4'Size use 32;

   type Word_2 is mod 2**16;
   for Word_2'Size use 16;

   type Word_1 is mod 256;
   for Word_1'Size use 8;

   type Page_Magic     is mod 2**32;
   type Page_Flags     is mod 2**16;
   type Page_Identity  is mod 2**16;

   type Page_Header is
      record
         Magic     : Page_Magic     := Header_Magic;
         Flags     : Page_Flags     := 0;
         Identity  : Page_Identity  := 0;
         Location  : File_And_Page  := 0;
      end record;

   type Page_Tail is
      record
         Unused    : Word_4;
         Magic     : Page_Magic     := Tail_Magic;
      end record;

   Page_Bits : constant := Page_Size * System.Storage_Unit;

   Page_Contents_Bits : constant :=
     Page_Bits - Marlowe.Sizes.Page_Header_Bits -
     Marlowe.Sizes.Page_Tail_Bits;

   Page_Contents_Units : constant :=
     Page_Contents_Bits / System.Storage_Unit;

   type Page_Contents is
     array (Slot_Index range 0 .. Page_Contents_Units - 1)
     of System.Storage_Elements.Storage_Element;
   for Page_Contents'Size use Page_Contents_Bits;

   for Page_Header'Size use Marlowe.Sizes.Page_Header_Bits;
   for Page_Tail'Size   use Marlowe.Sizes.Page_Tail_Bits;

   type Page_Record is
      record
         Header   : Page_Header;
         Contents : Page_Contents := (others => 254);
         Tail     : Page_Tail;
      end record;

   for Page_Record'Size use Page_Bits;

end Marlowe.Pages;
