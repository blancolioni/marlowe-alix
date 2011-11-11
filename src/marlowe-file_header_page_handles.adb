package body Marlowe.File_Header_Page_Handles is

   --------------------------
   -- Get_File_Header_Page --
   --------------------------

   function Get_File_Header_Page
     (Handle : File_Header_Page_Handle)
     return Marlowe.Pages.File_Header.File_Header_Page
   is
   begin
      return Handle.The_File_Header_Page;
   end Get_File_Header_Page;

   --------------------
   -- Get_File_Index --
   --------------------

   function Get_File_Index
     (Item : File_Header_Page_Handle)
     return File_Index
   is
   begin
      return Marlowe.Pages.File_Header.Get_File_Index
        (Item.The_File_Header_Page);
   end Get_File_Index;

   -------------------------
   -- Get_Last_File_Index --
   -------------------------

   function Get_Last_File_Index (Item : File_Header_Page_Handle)
                                return File_Index
   is
   begin
      return Marlowe.Pages.File_Header.Get_Last_File_Index
        (Item.The_File_Header_Page);
   end Get_Last_File_Index;


   -------------------------
   -- Set_Last_File_Index --
   -------------------------

   procedure Set_Last_File_Index (Item : File_Header_Page_Handle;
                                  To   : File_Index)
   is
   begin
      Set_Dirty (Item);
      Marlowe.Pages.File_Header.Set_Last_File_Index
        (Item.The_File_Header_Page, To);
   end Set_Last_File_Index;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page (Handle    : in out File_Header_Page_Handle;
                       Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.Set_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_File_Header_Page :=
        Marlowe.Pages.File_Header.To_File_Header_Page (Get_Page (Handle));
   end Set_Page;

end Marlowe.File_Header_Page_Handles;
