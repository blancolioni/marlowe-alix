with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Marlowe.Pages.Btree_Leaf is

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Item  : in Btree_Leaf_Page;
                     Index : in Slot_Index)
                    return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      pragma Assert (Index >= 1);
      pragma Assert (Index <= Number_Of_Keys (Item));
      Start_Offset : constant Slot_Index :=
        (Index - 1) * Item.Contents.Key_Length + 1;
      End_Offset   : constant Slot_Index :=
        Start_Offset + Item.Contents.Key_Length - 1;
   begin
      return Item.Contents.Contents (Storage_Count (Start_Offset) ..
                                     Storage_Count (End_Offset));
   end Get_Key;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Item : in Btree_Leaf_Page) return File_And_Page is
   begin
      return Item.Contents.Parent;
   end Get_Parent;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item           : in Btree_Leaf_Page;
                         Loc            : in File_And_Page;
                         Key_Length     : in Positive)
   is
   begin
      Initialise (To_Page (Item),
                  Marlowe.Page_Types.Btree_Leaf_Page_Type, Loc);
      Item.Contents :=
        (Parent     => 0,
         Key_Length => Slot_Index (Key_Length),
         Key_Count  => 0,
         Contents   => (others => 16#BA#));

   end Initialise;

   --------------------
   -- Minimum_Degree --
   --------------------

   function Minimum_Degree (Key_Length : Slot_Index) return Slot_Index is
      Max_Leaf_Keys : constant Slot_Index :=
        (Leaf_Contents_Bits / (System.Storage_Unit * Key_Length));
   begin
      return (Max_Leaf_Keys + 1) / 2;
   end Minimum_Degree;

   --------------------
   -- Minimum_Degree --
   --------------------

   function Minimum_Degree (Item : in Btree_Leaf_Page) return Slot_Index is
   begin
      return Minimum_Degree (Item.Contents.Key_Length);
   end Minimum_Degree;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys (Item : in Btree_Leaf_Page) return Slot_Index is
   begin
      return Item.Contents.Key_Count;
   end Number_Of_Keys;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out Btree_Leaf_Page) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Btree_Leaf_Page_Record,
                                         Btree_Leaf_Page);
   begin
      Free (Item);
   end Release;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key
     (Item  : in Btree_Leaf_Page;
      Index : in Slot_Index;
      Key   : in System.Storage_Elements.Storage_Array)
   is
      use System.Storage_Elements;
      pragma Assert (Slot_Index (Key'Length) = Item.Contents.Key_Length);
      pragma Assert (Index >= 1);
      pragma Assert (Index <= Number_Of_Keys (Item));
      Start_Offset : constant Slot_Index :=
        (Index - 1) * Item.Contents.Key_Length + 1;
      End_Offset   : constant Slot_Index :=
          Start_Offset + Item.Contents.Key_Length - 1;
   begin
      Item.Contents.Contents (Storage_Count (Start_Offset) ..
                              Storage_Count (End_Offset)) := Key;
   end Set_Key;

   ------------------------
   -- Set_Number_Of_Keys --
   ------------------------

   procedure Set_Number_Of_Keys
     (Item  : in Btree_Leaf_Page;
      Count : in Slot_Index)
   is
   begin
      Item.Contents.Key_Count := Count;
   end Set_Number_Of_Keys;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Item   : in Btree_Leaf_Page;
      Parent : in File_And_Page)
   is
   begin
      Item.Contents.Parent := Parent;
   end Set_Parent;

   --------------------------
   -- To_Btree_Leaf_Page --
   --------------------------

   function To_Btree_Leaf_Page (Item : Page) return Btree_Leaf_Page is
      use Marlowe.Page_Types;
      function TFHP is
         new Ada.Unchecked_Conversion (Page, Btree_Leaf_Page);
   begin
      pragma Assert (Get_Page_Type (Item) = Btree_Leaf_Page_Type or
                     Get_Page_Type (Item) = No_Page_Type);
      return TFHP (Item);
   end To_Btree_Leaf_Page;

   -------------
   -- To_Page --
   -------------

   function To_Page (Item : Btree_Leaf_Page) return Page is
      function TP is
         new Ada.Unchecked_Conversion (Btree_Leaf_Page, Page);
   begin
        return TP (Item);
   end To_Page;


end Marlowe.Pages.Btree_Leaf;
