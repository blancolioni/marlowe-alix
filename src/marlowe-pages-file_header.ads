with Marlowe.Version;

package Marlowe.Pages.File_Header is

   type File_Header_Page_Record is private;

   type File_Header_Page is access all File_Header_Page_Record;

   function To_File_Header_Page (Item : Page) return File_Header_Page;
   function To_Page (Item : File_Header_Page) return Page;

   procedure Initialise (Item       : in File_Header_Page;
                         File       : in File_Index;
                         File_Count : in File_Index);

   procedure Release (Item : in out File_Header_Page);

   function Get_File_Index (Item : File_Header_Page) return File_Index;
   function Get_Last_File_Index (Item : File_Header_Page) return File_Index;

   procedure Set_Last_File_Index (Item : File_Header_Page;
                                  To   : File_Index);

private

   type File_Header_Page_Contents is
      record
         Index           : File_Index;
         Last            : File_Index;
         Version_Major   : Word_1      := Marlowe.Version.Version_Major;
         Version_Minor   : Word_1      := Marlowe.Version.Version_Minor;
         Version_Release : Word_1      := Marlowe.Version.Version_Release;
         Version_Unused  : Word_1      := 0;
         Build_Number    : Word_4      := Marlowe.Version.Build_Number;
      end record;

   File_Header_Page_Contents_Bits : constant := 96;

   for File_Header_Page_Contents'Size use File_Header_Page_Contents_Bits;

   Spare_Bits : constant :=
     Page_Contents_Bits - File_Header_Page_Contents_Bits;
   Spare_Storage_Elements : constant :=
     Spare_Bits / System.Storage_Unit;

   type Unused_Storage is new
     System.Storage_Elements.Storage_Array (1 .. Spare_Storage_Elements);

   for Unused_Storage'Size use Spare_Bits;

   type File_Header_Page_Record is
      record
         Header    : Page_Header;
         Contents  : File_Header_Page_Contents;
         Unused    : Unused_Storage;
         Tail      : Page_Tail;
      end record;

   for File_Header_Page_Record'Size use Page_Bits;

end Marlowe.Pages.File_Header;

