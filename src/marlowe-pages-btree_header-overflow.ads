with System.Storage_Elements;

with Marlowe.Btree_Keys;

package Marlowe.Pages.Btree_Header.Overflow is

   type Btree_Header_Overflow_Page_Record is private;

   type Btree_Header_Overflow_Page is
     access all Btree_Header_Overflow_Page_Record;

   function To_Btree_Header_Overflow_Page
     (Item : Page)
     return Btree_Header_Overflow_Page;

   function To_Page (Item : Btree_Header_Overflow_Page) return Page;

   procedure Initialise (Item       : in Btree_Header_Overflow_Page;
                         Loc        : in File_And_Page);

   procedure Release (Item : in out Btree_Header_Overflow_Page);

   function Number_Of_Btrees
     (Item : Btree_Header_Overflow_Page)
     return Natural;

   function Get_Btree_Name (Item  : Btree_Header_Overflow_Page;
                            Index : Positive)
                           return String;

   function Get_Btree_Root (Item  : Btree_Header_Overflow_Page;
                            Index : Positive)
                           return File_And_Page;

   procedure Set_Btree_Root (Item     : in Btree_Header_Overflow_Page;
                             Index    : in Positive;
                             New_Root : in File_And_Page);

   function Get_Btree_Key (Item     : in Btree_Header_Overflow_Page;
                           Index    : in Positive)
                          return Marlowe.Btree_Keys.Component_Array;

   function Get_Btree_Key_Length (Item     : in Btree_Header_Overflow_Page;
                                  Index    : in Positive)
                                 return Positive;

   function Get_Node_Minimum_Degree (Item  : in Btree_Header_Overflow_Page;
                                     Index : in Positive)
                                    return Natural;

   function Get_Leaf_Minimum_Degree (Item  : in Btree_Header_Overflow_Page;
                                     Index : in Positive)
                                    return Natural;

   function Get_Key_Length (Item  : in Btree_Header_Overflow_Page;
                            Index : in Positive)
                           return Slot_Index;

   function Add_Btree_Description
     (To_Page  : Btree_Header_Overflow_Page;
      Name     : String;
      Key      : Marlowe.Btree_Keys.Component_Array)
     return Positive;

private

   Max_Name_Length : constant := 81;
   --  We will be able to support longer names, but the name will
   --  be somehow compressed

   type Key_Length is range 0 .. 4095;
   for Key_Length'Size use 12;

   type Key_Component_Count is range 0 .. 15;
   for Key_Component_Count'Size use 4;

   type Array_Of_Components is
     array (Key_Component_Count) of Marlowe.Btree_Keys.Key_Component;
   pragma Pack (Array_Of_Components);

   type Btree_Info_Record is
      record
         Root           : File_And_Page;
         Length         : Key_Length;
         Num_Components : Key_Component_Count;
         Component      : Array_Of_Components;
         Name_Length    : System.Storage_Elements.Storage_Element;
         Node_Degree    : Word_2;
         Leaf_Degree    : Word_2;
         Name           : String (1 .. Max_Name_Length);
      end record;

   for Btree_Info_Record use
      record
         Root           at  0 range  0 ..  63;
         Length         at  8 range  0 ..  11;
         Num_Components at  8 range 12 ..  15;
         Component      at 10 range  0 .. 255;
         Name_Length    at 42 range  0 ..   7;
         Node_Degree    at 43 range  0 ..  15;
         Leaf_Degree    at 45 range  0 ..  15;
         Name           at 47 range  0 ..  Max_Name_Length * 8 - 1;
      end record;

   for Btree_Info_Record'Size use 128 * System.Storage_Unit;

   type Btree_Info_Array is
     array (1 .. (Page_Contents_Bits - 96) / 128 / System.Storage_Unit) of
     Btree_Info_Record;
   pragma Pack (Btree_Info_Array);

   type Unused_Space is new
     System.Storage_Elements.Storage_Array (1 .. 96);

   type Btree_Header_Overflow_Page_Contents is
      record
         Overflow    : File_And_Page;
         Count       : Page_Index;
         Info        : Btree_Info_Array;
         Unused      : Unused_Space;
      end record;

   for Btree_Header_Overflow_Page_Contents'Size use Page_Contents_Bits;

   type Btree_Header_Overflow_Page_Record is
      record
         Header    : Page_Header;
         Contents  : Btree_Header_Overflow_Page_Contents;
         Tail      : Page_Tail;
      end record;

   for Btree_Header_Overflow_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Btree_Header;

