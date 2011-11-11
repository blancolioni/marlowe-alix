with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Marlowe.Page_Types;

package body Marlowe.Pages.File_Header is

   --------------------
   -- Get_File_Index --
   --------------------

   function Get_File_Index (Item : File_Header_Page) return File_Index is
   begin
      return Item.Contents.Index;
   end Get_File_Index;

   -------------------------
   -- Get_Last_File_Index --
   -------------------------

   function Get_Last_File_Index (Item : File_Header_Page) return File_Index is
   begin
      return Item.Contents.Last;
   end Get_Last_File_Index;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item       : in File_Header_Page;
                         File       : in File_Index;
                         File_Count : in File_Index)
   is
   begin
      Initialise (To_Page (Item),
                  Page_Types.File_Header_Page_Type,
                  To_File_And_Page (File, 1));
      Item.Contents.Index := File;
      Item.Contents.Last  := File_Count;
   end Initialise;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out File_Header_Page) is
      procedure Free is
         new Ada.Unchecked_Deallocation (File_Header_Page_Record,
                                         File_Header_Page);
   begin
      Free (Item);
   end Release;

   -------------------------
   -- Set_Last_File_Index --
   -------------------------

   procedure Set_Last_File_Index (Item : File_Header_Page;
                                  To   : File_Index)
   is
   begin
      Item.Contents.Last := To;
   end Set_Last_File_Index;

   -------------------------
   -- To_File_Header_Page --
   -------------------------

   function To_File_Header_Page (Item : Page) return File_Header_Page is
      use Marlowe.Page_Types;
      function TFHP is
         new Ada.Unchecked_Conversion (Page, File_Header_Page);
   begin
      pragma Assert (Get_Page_Type (Item) = File_Header_Page_Type);
      return TFHP (Item);
   end To_File_Header_Page;

   -------------
   -- To_Page --
   -------------

   function To_Page (Item : File_Header_Page) return Page is
      function TP is
         new Ada.Unchecked_Conversion (File_Header_Page, Page);
   begin
        return TP (Item);
   end To_Page;

end Marlowe.Pages.File_Header;

