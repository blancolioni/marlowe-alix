with System.Storage_Elements;

package Marlowe.Pages.Btree is

   type Btree_Page_Record is private;

   type Btree_Page is access all Btree_Page_Record;

   function To_Btree_Page (Item : Page) return Btree_Page;
   function To_Page (Item : Btree_Page) return Page;

   procedure Initialise (Item           : in Btree_Page);

   procedure Release (Item : in out Btree_Page);


   procedure Set_Page_Info
     (Item       : in Btree_Page;
      Leaf       : in Boolean;
      Key_Length : in Positive);
   --  Clumsy, but when Initialise is called, we don't have any
   --  of this information.

   function Is_Leaf (Item : in Btree_Page) return Boolean;
   function Get_Key_Length (Item : in Btree_Page) return Positive;

   function Node_Minimum_Degree
     (Key_Length : Slot_Index)
     return Slot_Index;

   function Leaf_Minimum_Degree
     (Key_Length : Slot_Index)
     return Slot_Index;

   function Minimum_Degree (Item : in Btree_Page) return Slot_Index;

   function Number_Of_Keys (Item : in Btree_Page) return Slot_Index;
   --  function Get_Key (Item  : in Btree_Page;
   --                    Index : in Slot_Index)
   --                   return System.Storage_Elements.Storage_Array;

   procedure Get_Key (Item   : in     Btree_Page;
                      Index  : in     Slot_Index;
                      Result :    out System.Storage_Elements.Storage_Array);

   function Find_Key_Forward
     (Item       : Btree_Page;
      Key        : System.Storage_Elements.Storage_Array)
      return Slot_Index
     with Inline_Always;

   function Get_Parent (Item : in Btree_Page) return File_And_Page;

   procedure Set_Key
     (Item  : in Btree_Page;
      Index : in Slot_Index;
      Key   : in System.Storage_Elements.Storage_Array);

   procedure Set_Number_Of_Keys
     (Item  : in Btree_Page;
      Count : in Slot_Index);

   procedure Set_Parent
     (Item   : in Btree_Page;
      Parent : in File_And_Page);

   function Get_Child (Item  : in Btree_Page;
                       Index : in Slot_Index)
                      return File_And_Page;

   procedure Set_Child (Item  : in Btree_Page;
                        Index : in Slot_Index;
                        Child : in File_And_Page);

private

   pragma Inline (Get_Key);
   pragma Inline (Get_Child);
   pragma Inline (Set_Parent);
   pragma Inline (To_Btree_Page);
   pragma Inline (To_Page);
   pragma Inline (Minimum_Degree);
   pragma Inline (Number_Of_Keys);
   pragma Inline (Set_Key);
   pragma Inline (Is_Leaf);
   pragma Inline (Find_Key_Forward);

   use type System.Storage_Elements.Storage_Offset;

   Btree_Page_Header_Size : constant := 112;

   Btree_Page_Contents_Bits : constant :=
     Page_Contents_Bits - Btree_Page_Header_Size;

   subtype Btree_Page_Contents_Array is
     System.Storage_Elements.Storage_Array
     (1 .. Btree_Page_Contents_Bits / System.Storage_Unit);

   type Btree_Page_Contents is
      record
         Parent      : File_And_Page;
         Key_Length  : Slot_Index;
         Key_Count   : Slot_Index;
         First_Child : Slot_Index;
         Contents    : Btree_Page_Contents_Array;
      end record;

   for Btree_Page_Contents'Size use Page_Contents_Bits;

   type Btree_Page_Record is
      record
         Header    : Page_Header;
         Contents  : Btree_Page_Contents;
         Tail      : Page_Tail;
      end record;

   for Btree_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Btree;

