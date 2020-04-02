with System.Storage_Elements;

with Marlowe.Pages.Btree;
with Marlowe.Page_Handles;

package Marlowe.Btree_Page_Handles is

   type Btree_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   overriding procedure Set_Page
     (Handle    : in out Btree_Page_Handle;
      Reference : in     File_And_Page);

   overriding procedure New_Page
     (Handle    : in out Btree_Page_Handle;
      Reference : in     File_And_Page);

   function Get_Btree_Page
     (Handle : Btree_Page_Handle'Class)
      return Marlowe.Pages.Btree.Btree_Page
     with Inline_Always;

   function Get_Key_Length (Item : in Btree_Page_Handle) return Positive;

   function Minimum_Degree (Item  : in Btree_Page_Handle)
                           return Slot_Index;

   function Minimum_Keys (Item : in Btree_Page_Handle) return Slot_Index;
   function Maximum_Keys (Item : in Btree_Page_Handle) return Slot_Index;

   function Number_Of_Keys (Item : in Btree_Page_Handle) return Slot_Index;

   function Get_Child (Item  : Btree_Page_Handle;
                       Index : Slot_Index)
                      return File_And_Page;

   function Get_Child (Item  : Btree_Page_Handle;
                       Index : Slot_Index)
                      return Btree_Page_Handle;

   --  function Get_Key (Item  : Btree_Page_Handle;
   --                    Index : Slot_Index)
   --                   return System.Storage_Elements.Storage_Array;

   procedure Get_Key (Item   : in     Btree_Page_Handle;
                      Index  : in     Slot_Index;
                      Result :    out System.Storage_Elements.Storage_Array);

   function Find_Key (Item       : Btree_Page_Handle;
                      Key        : System.Storage_Elements.Storage_Array;
                      Forward    : Boolean)
                     return Slot_Index;

   procedure Insert_Key (Item       : Btree_Page_Handle;
                         Key        : System.Storage_Elements.Storage_Array;
                         Child      : File_And_Page);

   procedure Insert_Key (Item       : Btree_Page_Handle;
                         Key        : System.Storage_Elements.Storage_Array);

   function Is_Leaf (Item : Btree_Page_Handle) return Boolean;
   function Is_Root (Item : Btree_Page_Handle) return Boolean;

   function Get_Parent (Item : Btree_Page_Handle) return File_And_Page;
   function Get_Parent (Item : Btree_Page_Handle) return Btree_Page_Handle;

   procedure Set_Parent (Item   : Btree_Page_Handle;
                         Parent : File_And_Page);

   procedure Set_Parent (Item   : Btree_Page_Handle;
                         Parent : Btree_Page_Handle);

   procedure Set_Key (Item  : Btree_Page_Handle;
                      Index : Slot_Index;
                      Key   : System.Storage_Elements.Storage_Array);

   procedure Move_Key (Item       : Btree_Page_Handle;
                       From_Index : Slot_Index;
                       To_Index   : Slot_Index);

   procedure Set_Child (Item  : Btree_Page_Handle;
                        Index : Slot_Index;
                        Child : File_And_Page);

   procedure Set_Child (Item  : Btree_Page_Handle;
                        Index : Slot_Index;
                        Child : Btree_Page_Handle);

   procedure Set_Number_Of_Keys (Item : Btree_Page_Handle;
                                 Num  : Slot_Index);

   procedure Delete_Key (Item  : Btree_Page_Handle;
                         Index : Slot_Index);

   procedure Split (Source   : in Btree_Page_Handle;
                    New_Page : in Btree_Page_Handle;
                    Index    : in Slot_Index;
                    Parent   : in Btree_Page_Handle);

   procedure Set_Page_Info
     (Item       : in Btree_Page_Handle;
      Leaf       : in Boolean;
      Key_Length : in Positive);

   procedure Dump_Page (Page      : Btree_Page_Handles.Btree_Page_Handle;
                        To_Output : Boolean := False);
   --  Write a page to the trace or to standard output

private

   pragma Inline (Get_Key);
   pragma Inline (Set_Key);
   pragma Inline (Is_Leaf);

   type Btree_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_Btree_Page : Marlowe.Pages.Btree.Btree_Page;
      end record;

end Marlowe.Btree_Page_Handles;
