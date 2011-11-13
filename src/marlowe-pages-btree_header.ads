with System.Storage_Elements;

package Marlowe.Pages.Btree_Header is

   type Btree_Header_Page_Record is private;

   type Btree_Header_Page is access all Btree_Header_Page_Record;

   function To_Btree_Header_Page (Item : Page) return Btree_Header_Page;
   function To_Page (Item : Btree_Header_Page) return Page;

   procedure Initialise (Item       : in Btree_Header_Page;
                         Loc        : in File_And_Page);

   procedure Release (Item : in out Btree_Header_Page);

   function Maximum_Btree_Count return Natural;
   --  How many B-Trees a header page can describe

   function Number_Of_Btrees (Item : Btree_Header_Page) return Natural;
   function Get_Btree_Name (Item  : Btree_Header_Page;
                            Index : Positive)
                           return String;
   function Get_Btree_Root (Item  : Btree_Header_Page;
                            Index : Positive)
                           return File_And_Page;

   procedure Set_Btree_Root (Item     : in Btree_Header_Page;
                             Index    : in Positive;
                             New_Root : in File_And_Page);

   function Get_Btree_Key_Length (Item     : in Btree_Header_Page;
                                  Index    : in Positive)
                                 return Positive;

   function Get_Node_Minimum_Degree (Item  : in Btree_Header_Page;
                                     Index : in Positive)
                                    return Natural;

   function Get_Leaf_Minimum_Degree (Item  : in Btree_Header_Page;
                                     Index : in Positive)
                                    return Natural;

   function Get_Key_Length (Item  : in Btree_Header_Page;
                            Index : in Positive)
                            return Slot_Index;

   function Get_Key_Table_Index (Item  : in Btree_Header_Page;
                                 Index : in Positive)
                                 return Table_Index;

   function Get_Table_Page (Item : in Btree_Header_Page) return File_And_Page;
   procedure Set_Table_Page (Item : in Btree_Header_Page;
                             Loc  : in File_And_Page);

   function Add_Btree_Description
     (To_Page    : Btree_Header_Page;
      Name       : String;
      Key_Length : System.Storage_Elements.Storage_Count)
     return Positive;

   function Get_Total_Btrees (For_Page : Btree_Header_Page) return Natural;
   procedure Increment_Total_Btrees (For_Page : Btree_Header_Page);

   function Get_Overflow_Page (From : Btree_Header_Page)
                              return File_And_Page;

   procedure Set_Overflow_Page (From          : Btree_Header_Page;
                                Overflow_Page : File_And_Page);

   procedure Set_Magic (Item  : Btree_Header_Page;
                        Magic : Btree_Magic);

   function Get_Magic (Item : Btree_Header_Page) return Btree_Magic;

private

   Max_Name_Length : constant := 47;
   --  We will be able to support longer names, but the name will
   --  be somehow compressed

   type Key_Length is range 0 .. 65535;
   for Key_Length'Size use 16;

   type Key_Component_Count is range 0 .. 15;
   for Key_Component_Count'Size use 4;

   type Btree_Info_Record is
      record
         Root           : File_And_Page;
         Length         : Key_Length;
         Node_Degree    : Word_2;
         Leaf_Degree    : Word_2;
         Record_Index   : Table_Index;
         Name_Length    : System.Storage_Elements.Storage_Element;
         Name           : String (1 .. Max_Name_Length);
      end record;

   for Btree_Info_Record use
      record
         Root           at  0 range  0 ..  63;
         Length         at  8 range  0 ..  15;
         Node_Degree    at 10 range  0 ..  15;
         Leaf_Degree    at 12 range  0 ..  15;
         Record_Index   at 14 range  0 ..  15;
         Name_Length    at 16 range  0 ..   7;
         Name           at 17 range  0 ..  Max_Name_Length * 8 - 1;
      end record;

   for Btree_Info_Record'Size use 64 * System.Storage_Unit;

   type Btree_Info_Array is
     array (1 .. (Page_Contents_Bits - 128) / 64 / System.Storage_Unit) of
     Btree_Info_Record;
   pragma Pack (Btree_Info_Array);

   type Unused_Space is new
     System.Storage_Elements.Storage_Array (1 .. 8);

   type Btree_Header_Page_Contents is
      record
         Magic        : Btree_Magic           := 0;
         Overflow     : File_And_Page;
         Tables       : File_And_Page;
         Count        : Page_Index;
         Total_Btrees : Word_4                := 0;
         Info         : Btree_Info_Array;
         Unused       : Unused_Space;
      end record;

   for Btree_Header_Page_Contents'Size use Page_Contents_Bits;

   type Btree_Header_Page_Record is
      record
         Header    : Page_Header;
         Contents  : Btree_Header_Page_Contents;
         Tail      : Page_Tail;
      end record;

   for Btree_Header_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Btree_Header;

