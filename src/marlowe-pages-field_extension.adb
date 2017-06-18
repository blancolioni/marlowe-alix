with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Marlowe.Page_Types;

package body Marlowe.Pages.Field_Extension is

   ----------------------------
   -- Field_Extension_Length --
   ----------------------------

   function Field_Extension_Length
     (Item : Field_Extension_Page)
      return System.Storage_Elements.Storage_Count
   is
   begin
      return System.Storage_Elements.Storage_Count
        (Item.Contents.Length);
   end Field_Extension_Length;

   ------------------
   -- Get_Contents --
   ------------------

   procedure Get_Contents
     (Item : Field_Extension_Page;
      Data : out System.Storage_Elements.Storage_Array)
   is
   begin
      Data := Item.Contents.Data (1 .. Field_Extension_Length (Item));
   end Get_Contents;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Item          : in Field_Extension_Page;
      Location      : in File_And_Page)
   is
   begin
      Initialise (To_Page (Item),
                  Page_Types.Field_Extension_Page_Type,
                  Location);
      Item.Contents.Overflow := 0;
      Item.Contents.Length   := 0;
      Item.Contents.Data     := (others => 0);

   end Initialise;

   -------------------
   -- Overflow_Page --
   -------------------

   function Overflow_Page
     (Item : Field_Extension_Page)
      return File_And_Page
   is
   begin
      return Item.Contents.Overflow;
   end Overflow_Page;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out Field_Extension_Page) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Field_Extension_Page_Record,
                                         Field_Extension_Page);
   begin
      Free (Item);
   end Release;

   ------------------
   -- Set_Contents --
   ------------------

   procedure Set_Contents
     (Item : Field_Extension_Page;
      Data : System.Storage_Elements.Storage_Array;
      Next : out System.Storage_Elements.Storage_Offset)
   is
      use System.Storage_Elements;
   begin
      if Data'Length <= Max_Extension_Data then
         Item.Contents.Data (1 .. Data'Length) := Data;
         Item.Contents.Length := Slot_Index (Data'Length);
         Next := Data'Last + 1;
      else
         Item.Contents.Data :=
           Data (Data'First .. Data'First + Max_Extension_Data - 1);
         Next := Data'First + Max_Extension_Data;
      end if;
   end Set_Contents;

   -----------------------
   -- Set_Overflow_Page --
   -----------------------

   procedure Set_Overflow_Page
     (Item     : Field_Extension_Page;
      Overflow : File_And_Page)
   is
   begin
      Item.Contents.Overflow := Overflow;
   end Set_Overflow_Page;

   -----------------------------
   -- To_Field_Extension_Page --
   -----------------------------

   function To_Field_Extension_Page
     (Item : Page)
      return Field_Extension_Page
   is
      use Marlowe.Page_Types;
      function TFHP is
         new Ada.Unchecked_Conversion (Page, Field_Extension_Page);
   begin
      pragma Assert (Get_Page_Type (Item) = Field_Extension_Page_Type);
      return TFHP (Item);
   end To_Field_Extension_Page;

   -------------
   -- To_Page --
   -------------

   function To_Page (Item : Field_Extension_Page) return Page is
      function TP is
         new Ada.Unchecked_Conversion (Field_Extension_Page, Page);
   begin
        return TP (Item);
   end To_Page;

end Marlowe.Pages.Field_Extension;
