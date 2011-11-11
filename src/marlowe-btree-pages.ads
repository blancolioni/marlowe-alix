with Ada.Calendar;
with Ada.Finalization;

with Marlowe.Btree.Disk_Pages;

generic
   type Key_Type is private;
   Minimum_Degree : Slot;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
package Marlowe.Btree.Pages is

   Locking_Error : exception;

   Min_Keys_In_Page : constant Slot := Minimum_Degree - 1;
   Max_Keys_In_Page : constant Slot := 2 * Minimum_Degree - 1;

   subtype Key_Count   is Slot range 0 .. Max_Keys_In_Page;
   subtype Key_Index   is Key_Count range 1 .. Key_Count'Last;
   subtype Child_Index is Slot range 1 .. Max_Keys_In_Page + 1;

   procedure Set_Cache_Size (To : Natural);

   type Btree_Page is private;

   function Null_Page return Btree_Page;

   function Get_Page (Index  : in     Page_Index) return Btree_Page;

   function Get_Root_Page return Btree_Page;

   function New_Page return Btree_Page;

   procedure Deallocate_Page (Page : in out Btree_Page);

   function Get_Child (From   : Btree_Page;
                       Index  : Child_Index)
                      return Page_Index;

   function Get_Child (From  : Btree_Page;
                       Index : Child_Index)
                      return Btree_Page;

   function Number_Of_Keys (In_Page : Btree_Page) return Key_Count;

   function Get_Key (From_Page : Btree_Page;
                     Index     : Key_Index)
                    return Key_Type;

   function Find_Key (In_Page : Btree_Page;
                      Key     : Key_Type;
                      Forward : Boolean     := True)
                     return Child_Index;

   function Is_Leaf (Page : Btree_Page) return Boolean;
   function Is_Root (Page : Btree_Page) return Boolean;

   procedure Set_Leaf (Page : in Btree_Page;
                       Leaf : in Boolean);

   function Get_Index  (Of_Page : Btree_Page) return Page_Index;
   function Get_Parent (Of_Page : Btree_Page) return Page_Index;
   function Get_Parent (Of_Page : Btree_Page) return Btree_Page;

   procedure Set_Parent (Of_Page : in Btree_Page;
                         Parent  : in Page_Index);

   procedure Set_Parent (Of_Page : in Btree_Page;
                         Parent  : in Btree_Page);

   procedure Initialise (Page   : in out Btree_Page;
                         Index  : in     Page_Index;
                         Leaf   : in     Boolean);

   procedure Insert (To_Page : in Btree_Page;
                     Key     : in Key_Type;
                     Child   : in Page_Index);

   procedure Set_Key (In_Page : in Btree_Page;
                      Index   : in Key_Index;
                      Key     : in Key_Type);

   procedure Set_Child (In_Page : in Btree_Page;
                        Index   : in Child_Index;
                        Child   : in Page_Index);

   procedure Set_Number_Of_Keys (In_Page : in Btree_Page;
                                 Num     : in Key_Count);

   procedure Set_Root (Page : in Btree_Page);

   procedure Delete_Key (From_Page : in Btree_Page;
                         Key       : in Key_Type);

   procedure Delete (From_Page : in Btree_Page;
                     Index     : in Child_Index);

   procedure Split (Source   : in out Btree_Page;
                    New_Sib  : in out Btree_Page;
                    Index    : in     Key_Index;
                    Parent   : in out Btree_Page);

   procedure Dump_Page (Page : in Btree_Page);

   type Lock_Type is (Clear, Shared, Update, Exclusive);

   procedure Lock (Page    : in out Btree_Page;
                   Success :    out Boolean;
                   Lock    : in     Lock_Type       := Shared;
                   Wait    : in     Boolean         := True);

   procedure Change_Lock (Page     : in out Btree_Page;
                          Success  :    out Boolean;
                          New_Lock : in     Lock_Type       := Shared;
                          Wait     : in     Boolean         := False);

   procedure Unlock (Page : in out Btree_Page;
                     Lock : in     Lock_Type);

   procedure Create (Path : String);
   procedure Open (Path : String);
   procedure Close;
   procedure Flush;

private

   package Disk_Pages is
      new Marlowe.Btree.Disk_Pages (Key_Type, Minimum_Degree,
                                   "=", "<");

   function Get_Page (From   : in     Disk_Pages.Btree_Disk_Page)
                     return Btree_Page;

   type Btree_Page_Access is access Disk_Pages.Btree_Disk_Page;

   type Lock_Status  is array (Lock_Type) of Boolean;
   type Lock_Waiting is array (Lock_Type) of Natural;

   type LRU_Element;
   type LRU_List is access LRU_Element;

   type LRU_Element is
     record
        Next, Prev : LRU_List;
        Element    : Page_Index;
     end record;

   type Page_Info_Record is
      record
         Cached      : Boolean             := True;
         Locked      : Lock_Status         := (others => False);
         Waiting     : Lock_Waiting        := (others => 0);
         X_Locked    : Boolean             := False;
         U_Locks     : Natural             := 0;
         S_Locks     : Natural             := 0;
         Dirty       : Boolean             := False;
         Last_Access : Ada.Calendar.Time;
         References  : Natural             := 1;
         LRU         : LRU_List            := null;
      end record;

   type Page_Info is access Page_Info_Record;

   type  Btree_Page is new Ada.Finalization.Controlled with
      record
         Info     : Page_Info;
         Page     : Btree_Page_Access;
      end record;

   procedure Initialize (Page : in out Btree_Page);
   procedure Finalize (Page : in out Btree_Page);
   procedure Adjust (Page : in out Btree_Page);

end Marlowe.Btree.Pages;
