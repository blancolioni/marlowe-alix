with Marlowe.Btree_Keys;
with Marlowe.Pages.Btree_Header;

with Marlowe.Page_Handles;

package Marlowe.Btree_Header_Page_Handles is

   type Btree_Header_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   procedure Set_Page (Handle    : in out Btree_Header_Page_Handle;
                       Reference : in     File_And_Page);

   procedure New_Page (Handle    : in out Btree_Header_Page_Handle;
                       Reference : in     File_And_Page);

   function Get_Btree_Header_Page
     (Handle : Btree_Header_Page_Handle)
     return Marlowe.Pages.Btree_Header.Btree_Header_Page;

   function Add_Btree_Description
     (To_Page  : Btree_Header_Page_Handle;
      Name     : String;
      Key      : Marlowe.Btree_Keys.Component_Array)
     return Positive;

   function Maximum_Btree_Count return Natural;

   function Number_Of_Btrees
     (Item : Btree_Header_Page_Handle)
     return Natural;

   function Get_Total_Btrees (For_Page : Btree_Header_Page_Handle)
                             return Natural;

   procedure Increment_Total_Btrees (For_Page : Btree_Header_Page_Handle);

   function Get_Overflow_Page (From : Btree_Header_Page_Handle)
                              return File_And_Page;

   function Get_Overflow_Page (From : Btree_Header_Page_Handle)
                              return Btree_Header_Page_Handle;

   procedure Set_Overflow_Page (Item          : Btree_Header_Page_Handle;
                                Overflow_Page : File_And_Page);

   function Get_Btree_Name (Item  : Btree_Header_Page_Handle;
                            Index : Positive)
                           return String;

   function Get_Btree_Root (Item  : Btree_Header_Page_Handle;
                            Index : Positive)
                           return File_And_Page;

   procedure Set_Btree_Root (Item     : in Btree_Header_Page_Handle;
                             Index    : in Positive;
                             New_Root : in File_And_Page);

   function Get_Btree_Key (Item     : in Btree_Header_Page_Handle;
                           Index    : in Positive)
                          return Marlowe.Btree_Keys.Component_Array;

   function Get_Btree_Key_Length (Item  : in Btree_Header_Page_Handle;
                                  Index : in Positive)
                                 return Positive;

   function Get_Node_Minimum_Degree (Item  : in Btree_Header_Page_Handle;
                                     Index : in Positive)
                                    return Natural;

   function Get_Leaf_Minimum_Degree (Item  : in Btree_Header_Page_Handle;
                                     Index : in Positive)
                                    return Natural;

   function Get_Key_Table_Index (Item  : in Btree_Header_Page_Handle;
                                 Index : in Positive)
                                 return Table_Index;

   function Get_Table_Page (Item : in Btree_Header_Page_Handle)
                            return File_And_Page;
   procedure Set_Table_Page (Item : in Btree_Header_Page_Handle;
                             Loc  : in File_And_Page);

   procedure Set_Magic (Item  : Btree_Header_Page_Handle;
                        Magic : Btree_Magic);

   function Get_Magic (Item : Btree_Header_Page_Handle) return Btree_Magic;

private

   type Btree_Header_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_Btree_Header_Page : Marlowe.Pages.Btree_Header.Btree_Header_Page;
      end record;

end Marlowe.Btree_Header_Page_Handles;
