with Marlowe.Page_Types;
with Marlowe.Page_Handles;
with Marlowe.Pages.Table;

generic
   Table_Page_Type : Marlowe.Page_Types.Page_Type;
   type Contents_Type is private;
   Default_Value : Contents_Type;
   with package Table_Pages is new Marlowe.Pages.Table (Table_Page_Type,
                                                        Contents_Type);
package Marlowe.Table_Page_Handles is

   type Table_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   procedure Set_Page (Handle    : in out Table_Page_Handle;
                       Reference : in     File_And_Page);

   function Get_Table_Page
     (Handle : Table_Page_Handle)
     return Table_Pages.Table_Page;

   procedure Get_Table_Value (Item   : in Table_Page_Handle;
                              Offset : in Slot_Index;
                              Value  : out Contents_Type);

   function Get_Table_Value (Item   : in Table_Page_Handle;
                             Offset : in Slot_Index)
                             return Contents_Type;

   procedure Set_Table_Value (Item   : in Table_Page_Handle;
                              Offset : Slot_Index;
                              Value  :  Contents_Type);

   overriding
   procedure New_Page (Handle    : in out Table_Page_Handle;
                       Reference : in     File_And_Page);

private

   type Table_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_Table_Page : Table_Pages.Table_Page;
      end record;

end Marlowe.Table_Page_Handles;
