generic
   type Key_Type is private;
   Minimum_Degree : Slot;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
package Marlowe.Btree.Disk_Pages is

   type Btree_Disk_Page is private;

   Min_Keys_In_Page : constant Slot := Minimum_Degree - 1;
   Max_Keys_In_Page : constant Slot := 2 * Minimum_Degree - 1;

   subtype Key_Count   is Slot range 0 .. Max_Keys_In_Page;
   subtype Key_Index   is Key_Count range 1 .. Key_Count'Last;
   subtype Child_Index is Slot range 1 .. Max_Keys_In_Page + 1;

   function Number_Of_Keys (In_Page : Btree_Disk_Page) return Key_Count;

   function Get_Key (From_Page : Btree_Disk_Page;
                     Index     : Key_Index)
                    return Key_Type;

   function Get_Child (From_Page : Btree_Disk_Page;
                       Index     : Child_Index)
                      return Page_Index;

   function Find_Key (In_Page : Btree_Disk_Page;
                      Key     : Key_Type;
                      Forward : Boolean     := True)
                     return Child_Index;

   function Is_Leaf (Page : Btree_Disk_Page) return Boolean;
   function Is_Root (Page : Btree_Disk_Page) return Boolean;

   procedure Set_Leaf (Page : in out Btree_Disk_Page;
                       Leaf : in     Boolean);

   procedure Set_Root (Page : in Page_Index);

   function Get_Index  (Of_Page : Btree_Disk_Page) return Page_Index;
   function Get_Parent (Of_Page : Btree_Disk_Page) return Page_Index;

   procedure Set_Parent (Of_Page : in out Btree_Disk_Page;
                         Parent  : in     Page_Index);

   procedure Initialise (Page   : in out Btree_Disk_Page;
                         Index  : in     Page_Index;
                         Leaf   : in     Boolean);

   procedure Insert (To_Page : in out Btree_Disk_Page;
                     Key     : in     Key_Type;
                     Child   : in     Page_Index);

   procedure Delete (From_Page : in out Btree_Disk_Page;
                     Index     : in     Child_Index);

   procedure Set_Key (In_Page : in out Btree_Disk_Page;
                      Index   : in     Key_Index;
                      Key     : in     Key_Type);

   procedure Set_Child (In_Page : in out Btree_Disk_Page;
                        Index   : in     Child_Index;
                        Child   : in     Page_Index);

   procedure Set_Number_Of_Keys (In_Page : in out Btree_Disk_Page;
                                 Num     : in     Key_Count);

   procedure Split (Source   : in out Btree_Disk_Page;
                    New_Page : in out Btree_Disk_Page;
                    Index    : in     Key_Index;
                    Parent   : in out Btree_Disk_Page);

   procedure Dump_Page (Page : in Btree_Disk_Page);

   procedure Read_Page (Index   : in     Page_Index;
                        Page    :    out Btree_Disk_Page);

   procedure Write_Page (Page   : in     Btree_Disk_Page);

   procedure New_Page (Page : out Btree_Disk_Page);

   procedure Create (Name : in     String);

   procedure Open (Name : in     String);

   procedure Close;

   function Root_Page return Page_Index;

private

   type Array_Of_Keys     is array (Key_Index) of Key_Type;
   type Array_Of_Children is array (Child_Index) of Page_Index;

   type Btree_Disk_Page is
      record
         Leaf     : Boolean;
         Index    : Page_Index   := Null_Page_Index;
         Parent   : Page_Index   := Null_Page_Index;
         Num_Keys : Key_Count    := 0;
         Keys     : Array_Of_Keys;
         Children : Array_Of_Children;
      end record;

end Marlowe.Btree.Disk_Pages;
