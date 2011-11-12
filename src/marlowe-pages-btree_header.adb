with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Marlowe.Pages.Btree;

package body Marlowe.Pages.Btree_Header is

   ---------------------------
   -- Add_Btree_Description --
   ---------------------------

   function Add_Btree_Description
     (To_Page    : Btree_Header_Page;
      Name       : String;
      Key_Length : System.Storage_Elements.Storage_Count)
     return Positive
   is
   begin
      pragma Assert (Number_Of_Btrees (To_Page) < Maximum_Btree_Count);
      To_Page.Contents.Count := To_Page.Contents.Count + 1;
      declare
         Info : Btree_Info_Record renames
           To_Page.Contents.Info (Positive (To_Page.Contents.Count));
      begin
         Info.Root   := 0;
         Info.Length := Marlowe.Pages.Btree_Header.Key_Length (Key_Length);
         Info.Node_Degree :=
           Word_2 (Marlowe.Pages.Btree.Node_Minimum_Degree
                   (Slot_Index (Info.Length)));
         Info.Leaf_Degree :=
           Word_2 (Marlowe.Pages.Btree.Leaf_Minimum_Degree
                   (Slot_Index (Info.Length)));

         Info.Name_Length :=
           System.Storage_Elements.Storage_Element (Name'Length);
         Info.Name (1 .. Name'Length) := Name;
      end;
      return Positive (To_Page.Contents.Count);
   end Add_Btree_Description;

   --------------------------
   -- Get_Btree_Key_Length --
   --------------------------

   function Get_Btree_Key_Length (Item     : in Btree_Header_Page;
                                  Index    : in Positive)
                                 return Positive
   is
      pragma Assert (Index <= Number_Of_Btrees (Item));
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      return Positive (Info.Length);
   end Get_Btree_Key_Length;

   --------------------
   -- Get_Btree_Name --
   --------------------

   function Get_Btree_Name (Item  : Btree_Header_Page;
                            Index : Positive)
                           return String
   is
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      pragma Assert (Index <= Number_Of_Btrees (Item));
      return Info.Name (1 .. Natural (Info.Name_Length));
   end Get_Btree_Name;

   --------------------
   -- Get_Btree_Name --
   --------------------

   function Get_Btree_Root (Item  : Btree_Header_Page;
                            Index : Positive)
                           return File_And_Page
   is
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      pragma Assert (Index <= Number_Of_Btrees (Item));
      return Info.Root;
   end Get_Btree_Root;

   --------------------
   -- Get_Key_Length --
   --------------------

   function Get_Key_Length (Item  : in Btree_Header_Page;
                            Index : in Positive)
                           return Slot_Index
   is
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      pragma Assert (Index <= Number_Of_Btrees (Item));
      return Slot_Index (Info.Length);
   end Get_Key_Length;

   -------------------------
   -- Get_Key_Table_Index --
   -------------------------

   function Get_Key_Table_Index (Item  : in Btree_Header_Page;
                                 Index : in Positive)
                                 return Table_Index
   is
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      pragma Assert (Index <= Number_Of_Btrees (Item));
      return Info.Record_Index;
   end Get_Key_Table_Index;

   -----------------------------
   -- Get_Leaf_Minimum_Degree --
   -----------------------------

   function Get_Leaf_Minimum_Degree (Item  : in Btree_Header_Page;
                                     Index : in Positive)
                                    return Natural
   is
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      pragma Assert (Index <= Number_Of_Btrees (Item));
      return Natural (Info.Leaf_Degree);
   end Get_Leaf_Minimum_Degree;

   ---------------
   -- Get_Magic --
   ---------------

   function Get_Magic (Item : Btree_Header_Page) return Btree_Magic is
   begin
      return Item.Contents.Magic;
   end Get_Magic;

   -----------------------------
   -- Get_Node_Minimum_Degree --
   -----------------------------

   function Get_Node_Minimum_Degree (Item  : in Btree_Header_Page;
                                     Index : in Positive)
                                    return Natural
   is
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      pragma Assert (Index <= Number_Of_Btrees (Item));
      return Natural (Info.Node_Degree);
   end Get_Node_Minimum_Degree;

   -----------------------
   -- Get_Overflow_Page --
   -----------------------

   function Get_Overflow_Page (From : Btree_Header_Page)
                              return File_And_Page
   is
   begin
      return From.Contents.Overflow;
   end Get_Overflow_Page;

   --------------------
   -- Get_Table_Page --
   --------------------

   function Get_Table_Page (Item  : Btree_Header_Page)
                            return File_And_Page
   is
   begin
      return Item.Contents.Tables;
   end Get_Table_Page;

   ----------------------
   -- Get_Total_Btrees --
   ----------------------

   function Get_Total_Btrees (For_Page : Btree_Header_Page) return Natural is
   begin
      return Natural (For_Page.Contents.Total_Btrees);
   end Get_Total_Btrees;

   ----------------------------
   -- Increment_Total_Btrees --
   ----------------------------

   procedure Increment_Total_Btrees (For_Page : Btree_Header_Page) is
   begin
      For_Page.Contents.Total_Btrees := For_Page.Contents.Total_Btrees + 1;
   end Increment_Total_Btrees;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item       : in Btree_Header_Page;
                         Loc        : in File_And_Page)
   is
   begin
      Initialise (To_Page (Item),
                  Marlowe.Page_Types.Btree_Header_Page_Type, Loc);
      Item.Contents.Count := 0;
      Item.Contents.Overflow := 0;
      Item.Contents.Total_Btrees := 0;
   end Initialise;

   -------------------------
   -- Maximum_Btree_Count --
   -------------------------

   function Maximum_Btree_Count return Natural is
   begin
      return Btree_Info_Array'Length;
   end Maximum_Btree_Count;

   ----------------------
   -- Number_Of_Btrees --
   ----------------------

   function Number_Of_Btrees (Item : Btree_Header_Page) return Natural is
   begin
      return Natural (Item.Contents.Count);
   end Number_Of_Btrees;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out Btree_Header_Page) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Btree_Header_Page_Record,
                                         Btree_Header_Page);
   begin
      Free (Item);
   end Release;

   --------------------
   -- Set_Btree_Root --
   --------------------

   procedure Set_Btree_Root (Item     : in Btree_Header_Page;
                             Index    : in Positive;
                             New_Root : in File_And_Page)
   is
      Info : Btree_Info_Record renames
        Item.Contents.Info (Index);
   begin
      pragma Assert (Index <= Number_Of_Btrees (Item));
      Info.Root := New_Root;
   end Set_Btree_Root;

   ---------------
   -- Set_Magic --
   ---------------

   procedure Set_Magic (Item  : Btree_Header_Page;
                        Magic : Btree_Magic)
   is
   begin
      Item.Contents.Magic := Magic;
   end Set_Magic;

   -----------------------
   -- Set_Overflow_Page --
   -----------------------

   procedure Set_Overflow_Page (From          : Btree_Header_Page;
                                Overflow_Page : File_And_Page)
   is
   begin
      From.Contents.Overflow := Overflow_Page;
   end Set_Overflow_Page;

   --------------------
   -- Set_Table_Page --
   --------------------

   procedure Set_Table_Page (Item : in Btree_Header_Page;
                             Loc  : in File_And_Page)
   is
   begin
      Item.Contents.Tables := Loc;
   end Set_Table_Page;

   --------------------------
   -- To_Btree_Header_Page --
   --------------------------

   function To_Btree_Header_Page (Item : Page) return Btree_Header_Page is
      use Marlowe.Page_Types;
      function TFHP is
         new Ada.Unchecked_Conversion (Page, Btree_Header_Page);
   begin
      pragma Assert (Get_Page_Type (Item) = Btree_Header_Page_Type or
                     Get_Page_Type (Item) = No_Page_Type);
      return TFHP (Item);
   end To_Btree_Header_Page;

   -------------
   -- To_Page --
   -------------

   function To_Page (Item : Btree_Header_Page) return Page is
      function TP is
         new Ada.Unchecked_Conversion (Btree_Header_Page, Page);
   begin
        return TP (Item);
   end To_Page;

end Marlowe.Pages.Btree_Header;
