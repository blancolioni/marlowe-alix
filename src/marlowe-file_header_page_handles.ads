with Marlowe.Pages.File_Header;

with Marlowe.Page_Handles;

package Marlowe.File_Header_Page_Handles is

   type File_Header_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   procedure Set_Page (Handle    : in out File_Header_Page_Handle;
                       Reference : in     File_And_Page);

   function Get_File_Header_Page
     (Handle : File_Header_Page_Handle)
     return Marlowe.Pages.File_Header.File_Header_Page;

   function Get_File_Index (Item : File_Header_Page_Handle) return File_Index;
   function Get_Last_File_Index (Item : File_Header_Page_Handle)
                                return File_Index;

   procedure Set_Last_File_Index (Item : File_Header_Page_Handle;
                                  To   : File_Index);

private

   type File_Header_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_File_Header_Page : Marlowe.Pages.File_Header.File_Header_Page;
      end record;

end Marlowe.File_Header_Page_Handles;
