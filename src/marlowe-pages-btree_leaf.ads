with System.Storage_Elements;

package Marlowe.Pages.Btree_Leaf is

   type Btree_Leaf_Page_Record is private;

   type Btree_Leaf_Page is access all Btree_Leaf_Page_Record;

   function To_Btree_Leaf_Page (Item : Page) return Btree_Leaf_Page;
   function To_Page (Item : Btree_Leaf_Page) return Page;

   procedure Initialise (Item           : in Btree_Leaf_Page;
                         Loc            : in File_And_Page;
                         Key_Length     : in Positive);

   procedure Release (Item : in out Btree_Leaf_Page);

   function Minimum_Degree (Key_Length : Slot_Index) return Slot_Index;

   function Minimum_Degree (Item : in Btree_Leaf_Page) return Slot_Index;

   function Number_Of_Keys (Item : in Btree_Leaf_Page) return Slot_Index;
   function Get_Key (Item  : in Btree_Leaf_Page;
                     Index : in Slot_Index)
                    return System.Storage_Elements.Storage_Array;

   function Get_Parent (Item : in Btree_Leaf_Page) return File_And_Page;

   procedure Set_Key
     (Item  : in Btree_Leaf_Page;
      Index : in Slot_Index;
      Key   : in System.Storage_Elements.Storage_Array);

   procedure Set_Number_Of_Keys
     (Item  : in Btree_Leaf_Page;
      Count : in Slot_Index);

   procedure Set_Parent
     (Item   : in Btree_Leaf_Page;
      Parent : in File_And_Page);

private

   use type System.Storage_Elements.Storage_Offset;

   Leaf_Contents_Bits : constant := Page_Contents_Bits - 96;

   subtype Leaf_Page_Contents is
     System.Storage_Elements.Storage_Array
     (1 .. Leaf_Contents_Bits / System.Storage_Unit);

   type Btree_Leaf_Page_Contents is
      record
         Parent      : File_And_Page;
         Key_Length  : Slot_Index;
         Key_Count   : Slot_Index;
         Contents    : Leaf_Page_Contents;
      end record;

   for Btree_Leaf_Page_Contents'Size use Page_Contents_Bits;

   type Btree_Leaf_Page_Record is
      record
         Header    : Page_Header;
         Contents  : Btree_Leaf_Page_Contents;
         Tail      : Page_Tail;
      end record;

   for Btree_Leaf_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Btree_Leaf;

