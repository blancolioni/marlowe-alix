with System.Storage_Elements;

with Marlowe.Page_Handles;

with Marlowe.Pages.Field_Extension;

package Marlowe.Handles.Field_Extension_Page_Handles is

   type Field_Extension_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   procedure Set_Page (Handle    : in out Field_Extension_Page_Handle;
                       Reference : in     File_And_Page);

   overriding
   procedure New_Page (Handle    : in out Field_Extension_Page_Handle;
                       Reference : in     File_And_Page);

   function Extension_Data
     (Handle : Field_Extension_Page_Handle)
      return System.Storage_Elements.Storage_Array;

   procedure Set_Extension_Data
     (Handle : Field_Extension_Page_Handle;
      Data : System.Storage_Elements.Storage_Array;
      Next : out System.Storage_Elements.Storage_Offset);

   function Overflow_Page
     (Handle : Field_Extension_Page_Handle)
      return File_And_Page;

   procedure Set_Overflow_Page
     (Handle   : Field_Extension_Page_Handle;
      Overflow : File_And_Page);

private

   type Field_Extension_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_Extension_Page
            : Marlowe.Pages.Field_Extension.Field_Extension_Page;
      end record;

end Marlowe.Handles.Field_Extension_Page_Handles;
