package body Marlowe.Handles.Field_Extension_Page_Handles is

   --------------------
   -- Extension_Data --
   --------------------

   function Extension_Data
     (Handle : Field_Extension_Page_Handle)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      use Marlowe.Pages.Field_Extension;
      Result : Storage_Array
        (1 .. Field_Extension_Length (Handle.The_Extension_Page));
   begin
      Get_Contents (Handle.The_Extension_Page, Result);
      return Result;
   end Extension_Data;

   --------------
   -- New_Page --
   --------------

   overriding procedure New_Page
     (Handle    : in out Field_Extension_Page_Handle;
      Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.New_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Extension_Page :=
        Marlowe.Pages.Field_Extension.To_Field_Extension_Page
          (Get_Page (Handle));
      Marlowe.Pages.Field_Extension.Initialise
        (Item     => Handle.The_Extension_Page,
         Location => Reference);
      Set_Dirty (Handle);
   end New_Page;

   -------------------
   -- Overflow_Page --
   -------------------

   function Overflow_Page
     (Handle : Field_Extension_Page_Handle)
      return File_And_Page
   is
   begin
      return Marlowe.Pages.Field_Extension.Overflow_Page
        (Handle.The_Extension_Page);
   end Overflow_Page;

   ------------------------
   -- Set_Extension_Data --
   ------------------------

   procedure Set_Extension_Data
     (Handle : Field_Extension_Page_Handle;
      Data : System.Storage_Elements.Storage_Array;
      Next : out System.Storage_Elements.Storage_Offset)
   is
   begin
      Marlowe.Pages.Field_Extension.Set_Contents
        (Handle.The_Extension_Page, Data, Next);
      Handle.Set_Dirty;
   end Set_Extension_Data;

   -----------------------
   -- Set_Overflow_Page --
   -----------------------

   procedure Set_Overflow_Page
     (Handle   : Field_Extension_Page_Handle;
      Overflow : File_And_Page)
   is
   begin
      Marlowe.Pages.Field_Extension.Set_Overflow_Page
        (Handle.The_Extension_Page, Overflow);
      Handle.Set_Dirty;
   end Set_Overflow_Page;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page
     (Handle    : in out Field_Extension_Page_Handle;
      Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.Set_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Extension_Page :=
        Marlowe.Pages.Field_Extension.To_Field_Extension_Page
          (Get_Page (Handle));
   end Set_Page;

end Marlowe.Handles.Field_Extension_Page_Handles;
