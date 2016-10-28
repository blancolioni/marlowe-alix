with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Marlowe.Page_Types;

package body Marlowe.Pages.Table is

   ---------------------
   -- Get_Table_Value --
   ---------------------

   function Get_Table_Value
     (Item   : in Table_Page;
      Offset : Slot_Index)
      return Contents_Type
   is
   begin
      return Item.Contents (Offset);
   end Get_Table_Value;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Item          : in Table_Page;
      Location      : in File_And_Page)
   is
   begin
      Initialise (To_Page (Item), Table_Page_Type, Location);
   end Initialise;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out Table_Page) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Table_Page_Record,
                                         Table_Page);
   begin
      Free (Item);
   end Release;

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header (Item   : Table_Page;
                         Header : Header_Type)
   is
   begin
      Item.Table_Header := Header;
   end Set_Header;

   ---------------------
   -- Set_Table_Value --
   ---------------------

   procedure Set_Table_Value
     (Item   : in Table_Page;
      Offset : Slot_Index;
      Value  :  Contents_Type)
   is
   begin
      Item.Contents (Offset) := Value;
   end Set_Table_Value;

   -------------
   -- To_Page --
   -------------

   function To_Page (Item : Table_Page) return Page is
      function TP is new Ada.Unchecked_Conversion (Table_Page, Page);
   begin
      return TP (Item);
   end To_Page;

   -------------------
   -- To_Table_Page --
   -------------------

   function To_Table_Page (Item : Page) return Table_Page is
      use Marlowe.Page_Types;
      function TFM is new Ada.Unchecked_Conversion (Page, Table_Page);
   begin
      pragma Assert (Get_Page_Type (Item) = Table_Page_Type
                     or else Get_Page_Type (Item) = No_Page_Type);
      return TFM (Item);
   end To_Table_Page;

end Marlowe.Pages.Table;
