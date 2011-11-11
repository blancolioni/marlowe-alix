with Marlowe.Pages;

private package Marlowe.File_Header_Pages is

   type File_Header_Page_Record is private;

   type File_Header_Page is access all File_Header_Page_Record;

   function To_File_Header_Page (Item : Page) return File_Header_Page;
   function To_Page (Item : File_Header_Page) return Page;

   type File_Header_Page_Type is (Main_File_Header, Sub_File_Header);

   procedure Initialise (Item      :    out File_Header_Page_Record;
                         File      : in     File_Index;
                         Page_Type : in     File_Header_Page_Type);

private

   type File_Header_Page_Record is
      record
         Header    : Page_Header;
         Page_Type : File_Header_Page_Type;
      end record;

   for File_Header_Page_Record'Size use Page_Bits;

end Marlowe.Pages.File_Header;

