with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Marlowe.Page_Types;

package body Marlowe.Pages.Btree is

   pragma Style_Checks (Off);
   pragma No_Strict_Aliasing (Btree_Page);

   type File_And_Page_Storage is
     new System.Storage_Elements.Storage_Array (1 .. 8);

   function To_File_And_Page_Storage is
      new Ada.Unchecked_Conversion (File_And_Page,
                                    File_And_Page_Storage);
   function To_File_And_Page is
      new Ada.Unchecked_Conversion (File_And_Page_Storage,
                                    File_And_Page);

   --------------------------
   -- To_Btree_Page --
   --------------------------

   function To_Btree_Page (Item : Page) return Btree_Page is
      use Marlowe.Page_Types;
      function TFHP is
         new Ada.Unchecked_Conversion (Page, Btree_Page);
   begin
      pragma Assert (Get_Page_Type (Item) = Btree_Leaf_Page_Type or
                     Get_Page_Type (Item) = Btree_Node_Page_Type or
                     Get_Page_Type (Item) = No_Page_Type);
      return TFHP (Item);
   end To_Btree_Page;

   -------------
   -- To_Page --
   -------------

   function To_Page (Item : Btree_Page) return Page is
      function TP is
         new Ada.Unchecked_Conversion (Btree_Page, Page);
   begin
        return TP (Item);
   end To_Page;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Item : in Btree_Page) return Boolean is
      use Marlowe.Page_Types;
   begin
      return Get_Page_Type (To_Page (Item)) = Btree_Leaf_Page_Type;
   end Is_Leaf;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys (Item : in Btree_Page) return Slot_Index is
   begin
      return Item.Contents.Key_Count;
   end Number_Of_Keys;

   ----------------------
   -- Find_Key_Forward --
   ----------------------

   function Find_Key_Forward
     (Item       : Btree_Page;
      Key        : System.Storage_Elements.Storage_Array)
      return Slot_Index
   is
      use System.Storage_Elements;
      Start     : Storage_Offset := 1;
      Index     : Storage_Offset := 1;
      Key_Count  : constant Slot_Index := Item.Contents.Key_Count;
      Key_Length : constant Storage_Count :=
                     Storage_Count (Item.Contents.Key_Length);
      Key_Element : Storage_Element;
      Page_Element : Storage_Element;
   begin
      for Result in 1 .. Key_Count loop
         for I in Key'Range loop
            Key_Element := Key (I);
            Page_Element := Item.Contents.Contents (Index);
            if Key_Element > Page_Element then
               Start := Start + Key_Length;
               Index := Start;
               exit;
            elsif Key_Element < Page_Element then
               return Result;
            elsif I = Key'Last then
               return Result;
            else
               Index := Index + 1;
            end if;
         end loop;
      end loop;

      return Key_Count + 1;

   end Find_Key_Forward;

   -------------
   -- Get_Key --
   -------------

   procedure Get_Key (Item   : in     Btree_Page;
                      Index  : in     Slot_Index;
                      Result :    out System.Storage_Elements.Storage_Array)
   is
      use System.Storage_Elements;
      pragma Assert (Index >= 1);
      pragma Assert (Index <= Number_Of_Keys (Item));
      pragma Assert (Result'Length = Item.Contents.Key_Length);
      Start_Offset : constant Slot_Index :=
        (Index - 1) * Item.Contents.Key_Length + 1;
      End_Offset   : constant Slot_Index :=
        Start_Offset + Item.Contents.Key_Length - 1;
   begin
      Result :=
        Item.Contents.Contents (Storage_Count (Start_Offset) ..
                                Storage_Count (End_Offset));
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

--     function Get_Key (Item  : in Btree_Page;
--                       Index : in Slot_Index)
--                      return System.Storage_Elements.Storage_Array
--     is
--        use System.Storage_Elements;
--        pragma Assert (Index >= 1);
--        pragma Assert (Index <= Number_Of_Keys (Item));
--        Start_Offset : constant Slot_Index :=
--          (Index - 1) * Item.Contents.Key_Length + 1;
--        End_Offset   : constant Slot_Index :=
--          Start_Offset + Item.Contents.Key_Length - 1;
--     begin
--        return Item.Contents.Contents (Storage_Count (Start_Offset) ..
--                                       Storage_Count (End_Offset));
--     end Get_Key;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Item  : in Btree_Page;
                       Index : in Slot_Index)
                      return File_And_Page
   is
      use System.Storage_Elements;
      pragma Assert (Index >= 1);
      pragma Assert (Index <= Number_Of_Keys (Item) + 1);
      Start_Offset : constant Slot_Index :=
        Item.Contents.First_Child + (Index - 1) * 8 + 1;
      End_Offset   : constant Slot_Index :=
        Start_Offset + 8 - 1;
   begin
      if Is_Leaf (Item) then
         return 0;
      else
         return To_File_And_Page (File_And_Page_Storage
                                  (Item.Contents.Contents
                                   (Storage_Count (Start_Offset) ..
                                    Storage_Count (End_Offset))));
      end if;
   end Get_Child;

   --------------------
   -- Get_Key_Length --
   --------------------

   function Get_Key_Length (Item : in Btree_Page) return Positive is
   begin
      return Positive (Item.Contents.Key_Length);
   end Get_Key_Length;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Item : in Btree_Page) return File_And_Page is
   begin
      return Item.Contents.Parent;
   end Get_Parent;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item           : in Btree_Page) is
      pragma Unreferenced (Item);
   begin
      null;
   end Initialise;

   -------------------------
   -- Leaf_Minimum_Degree --
   -------------------------

   function Leaf_Minimum_Degree
     (Key_Length : Slot_Index)
     return Slot_Index
   is
      Max_Leaf_Keys : constant Slot_Index :=
        (Btree_Page_Contents_Bits / (System.Storage_Unit * Key_Length));
   begin
      return (Max_Leaf_Keys + 1) / 2;
   end Leaf_Minimum_Degree;

   --------------------
   -- Minimum_Degree --
   --------------------

   function Minimum_Degree (Item : in Btree_Page) return Slot_Index is
   begin
      if Is_Leaf (Item) then
         return Leaf_Minimum_Degree (Item.Contents.Key_Length);
      else
         return Node_Minimum_Degree (Item.Contents.Key_Length);
      end if;
   end Minimum_Degree;

   -------------------------
   -- Node_Minimum_Degree --
   -------------------------

   function Node_Minimum_Degree
     (Key_Length : Slot_Index)
     return Slot_Index
   is
      Max_Node_Keys : constant Slot_Index :=
        (Btree_Page_Contents_Bits - File_And_Page'Size) /
        (System.Storage_Unit * Key_Length + File_And_Page'Size);
   begin
      return (Max_Node_Keys + 1) / 2;
   end Node_Minimum_Degree;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out Btree_Page) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Btree_Page_Record,
                                         Btree_Page);
   begin
      Free (Item);
   end Release;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (Item  : in Btree_Page;
                        Index : in Slot_Index;
                        Child : in File_And_Page)
   is
      use System.Storage_Elements;
      pragma Assert (Index >= 1);
      pragma Assert (Index <= Number_Of_Keys (Item) + 1);
      Start_Offset : constant Slot_Index :=
        Item.Contents.First_Child + (Index - 1) * 8 + 1;
      End_Offset   : constant Slot_Index :=
        Start_Offset + 8 - 1;
   begin
      Item.Contents.Contents (Storage_Count (Start_Offset) ..
                              Storage_Count (End_Offset)) :=
        Storage_Array (To_File_And_Page_Storage (Child));
   end Set_Child;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key
     (Item  : in Btree_Page;
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
     (Item  : in Btree_Page;
      Count : in Slot_Index)
   is
   begin
      Item.Contents.Key_Count := Count;
   end Set_Number_Of_Keys;

   -------------------
   -- Set_Page_Info --
   -------------------

   procedure Set_Page_Info
     (Item       : in Btree_Page;
      Leaf       : in Boolean;
      Key_Length : in Positive)
   is
   begin
      if Leaf then
         Item.Header.Identity :=
           Page_Type'Pos
             (Marlowe.Page_Types.Btree_Leaf_Page_Type);
      else
         Item.Header.Identity :=
           Page_Type'Pos
             (Marlowe.Page_Types.Btree_Node_Page_Type);
      end if;

      Item.Contents :=
        (Parent      => 0,
         Key_Length  => Slot_Index (Key_Length),
         Key_Count   => 0,
         First_Child => 0,
         Contents    => (others => 16#BA#));

      if not Leaf then
         Item.Contents.First_Child :=
           Item.Contents.Key_Length *
           (2 * Node_Minimum_Degree (Item.Contents.Key_Length) - 1);
      end if;

   end Set_Page_Info;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Item   : in Btree_Page;
      Parent : in File_And_Page)
   is
   begin
      Item.Contents.Parent := Parent;
   end Set_Parent;


end Marlowe.Pages.Btree;
