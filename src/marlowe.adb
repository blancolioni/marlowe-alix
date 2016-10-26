package body Marlowe is

   --------------
   -- Get_File --
   --------------

   function Get_File (From : File_And_Page) return File_Index is
   begin
      return File_Index (From / (2 ** Page_Index_Bits));
   end Get_File;

   --------------
   -- Get_File --
   --------------

   function Get_File (From : File_Page_And_Slot) return File_Index is
   begin
      return File_Index (From / (2 ** (Page_Index_Bits + Slot_Index_Bits)));
   end Get_File;

   -----------------------
   -- Get_File_And_Page --
   -----------------------

   function Get_File_And_Page
     (From : File_Page_And_Slot)
      return File_And_Page
   is
   begin
      return To_File_And_Page (Get_File (From), Get_Page (From));
   end Get_File_And_Page;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (From : File_And_Page) return Page_Index is
   begin
      return Page_Index (From mod (2 ** Page_Index_Bits));
   end Get_Page;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (From : File_Page_And_Slot) return Page_Index is
   begin
      return Page_Index ((From / (2**Slot_Index_Bits))
                          mod (2**Page_Index_Bits));
   end Get_Page;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot (From : File_Page_And_Slot) return Slot_Index is
   begin
      return Slot_Index (From mod (2**Slot_Index_Bits));
   end Get_Slot;

   -----------
   -- Image --
   -----------

   function Image (Item : File_And_Page) return String is
      File_Image : constant String :=
        File_Index'Image (Get_File (Item));
      Page_Image : constant String :=
        Page_Index'Image (Get_Page (Item));
   begin
      return '[' & File_Image (2 .. File_Image'Last) & '/' &
        Page_Image (2 .. Page_Image'Last) & ']';
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Item : File_Page_And_Slot) return String is
      File_Image : constant String :=
        File_Index'Image (Get_File (Item));
      Page_Image : constant String :=
        Page_Index'Image (Get_Page (Item));
      Slot_Image : constant String :=
                     Slot_Index'Image (Get_Slot (Item));
   begin
      return '[' &
      File_Image (2 .. File_Image'Last) & '/' &
      Page_Image (2 .. Page_Image'Last) & '/' &
      Slot_Image (2 .. Slot_Image'Last) &
      ']';
   end Image;

   ----------------------
   -- To_File_And_Page --
   ----------------------

   function To_File_And_Page (File : File_Index;
                              Page : Page_Index)
                             return File_And_Page
   is
   begin
      return File_And_Page (File) * (2**Page_Index_Bits) +
        File_And_Page (Page);
   end To_File_And_Page;

   ---------------------------
   -- To_File_Page_And_Slot --
   ---------------------------

   function To_File_Page_And_Slot (File : File_Index;
                                   Page : Page_Index;
                                   Slot : Slot_Index)
                                  return File_Page_And_Slot
   is
   begin
      return File_Page_And_Slot (File) * 2**48 +
        File_Page_And_Slot (Page) * 2**16 +
        File_Page_And_Slot (Slot);
   end To_File_Page_And_Slot;

   ---------------------------
   -- To_File_Page_And_Slot --
   ---------------------------

   function To_File_Page_And_Slot (Page : File_And_Page;
                                   Slot : Slot_Index)
                                  return File_Page_And_Slot
   is
   begin
      return To_File_Page_And_Slot (Get_File (Page), Get_Page (Page), Slot);
   end To_File_Page_And_Slot;

end Marlowe;
