with System.Storage_Elements;

with Marlowe.Pages.Data_Definition;
with Marlowe.Page_Handles;

package Marlowe.Btree.Data_Definition_Handles is

   type Data_Definition_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   procedure Set_Page (Handle    : in out Data_Definition_Handle;
                       Reference : in     File_And_Page);

   procedure New_Page (Handle    : in out Data_Definition_Handle;
                       Reference : in     File_And_Page);

   function Get_Data_Definition_Page
     (Handle : Data_Definition_Handle)
     return Marlowe.Pages.Data_Definition.Data_Definition_Page;

   function Add_Record_Definition
     (To_Page  : Data_Definition_Handle;
      Name     : String;
      Length   : System.Storage_Elements.Storage_Count)
     return Table_Index;

   function Get_Next_Record_Overflow
     (Item  : in Data_Definition_Handle;
      Index : in Real_Table_Index)
      return File_Page_And_Slot;

   procedure Set_Next_Record_Overflow
     (Item  : in Data_Definition_Handle;
      Index : in Real_Table_Index;
      Addr  : in File_Page_And_Slot);

   function Maximum_Data_Record_Count return Natural;
   --  How many data records a header page can describe

   function Number_Of_Data_Records
     (Item : Data_Definition_Handle)
      return Natural;

   function Get_Record_Name (Item  : Data_Definition_Handle;
                             Index : Table_Index)
                             return String;

   function Get_Record_Root (Item  : Data_Definition_Handle;
                             Index : Table_Index)
                             return File_And_Page;

   procedure Set_Record_Root (Item     : in Data_Definition_Handle;
                              Index    : in Table_Index;
                              New_Root : in File_And_Page);

   function Allocate_Record (Item    : Data_Definition_Handle;
                             Index   : Table_Index)
                            return Database_Index;

   function Last_Record_Index (Item  : Data_Definition_Handle;
                               Table : Table_Index)
                              return Database_Index;

   function Get_Record_Length (Item     : in Data_Definition_Handle;
                               Index    : in Table_Index)
                               return System.Storage_Elements.Storage_Count;

   function Get_Overflow_Page (From : Data_Definition_Handle)
                              return File_And_Page;

   function Get_Overflow_Page (From : Data_Definition_Handle)
                              return Data_Definition_Handle;

   procedure Set_Overflow_Page (Item          : Data_Definition_Handle;
                                Overflow_Page : File_And_Page);

   procedure Set_First_Table_Index (Item : Data_Definition_Handle;
                                    Index : Table_Index);

private

   type Data_Definition_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_Data_Definition_Page :
             Marlowe.Pages.Data_Definition.Data_Definition_Page;
      end record;

   function Local_Index
     (Handle : Data_Definition_Handle;
      Index  : Table_Index)
      return Marlowe.Pages.Data_Definition.Local_Record_Index;

end Marlowe.Btree.Data_Definition_Handles;
