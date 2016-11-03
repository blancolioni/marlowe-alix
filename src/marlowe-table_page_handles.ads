with Marlowe.Page_Handles;
with Marlowe.Pages.Table;

generic
   Table_Page_Type : Page_Type;
   type Header_Type is private;
   type Contents_Type is private;
package Marlowe.Table_Page_Handles is

   type Table_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   overriding procedure Set_Page
     (Handle    : in out Table_Page_Handle;
      Reference : in     File_And_Page);

--     function Get_Table_Page
--       (Handle : Table_Page_Handle)
--       return Table_Pages.Table_Page;

   function Get_Header (Table : Table_Page_Handle) return Header_Type;
   procedure Set_Header (Table : Table_Page_Handle;
                         Header : Header_Type);

   procedure Get_Table_Value (Item   : in Table_Page_Handle;
                              Offset : in Slot_Index;
                              Value  : out Contents_Type);

   function Get_Table_Value (Item   : in Table_Page_Handle;
                             Offset : in Slot_Index)
                             return Contents_Type;

   procedure Set_Table_Value (Item   : in Table_Page_Handle;
                              Offset : Slot_Index;
                              Value  :  Contents_Type);

   Page_Slot_Count : constant Slot_Index;

   function Last_Slot (Item : Table_Page_Handle'Class) return Slot_Index;

   procedure Scan_Table
     (Item    : Table_Page_Handle'Class;
      Process : not null access
        procedure (Slot : Slot_Index;
                   Value : Contents_Type));

   overriding
   procedure New_Page (Handle    : in out Table_Page_Handle;
                       Reference : in     File_And_Page);

private

   package Table_Pages is
     new Marlowe.Pages.Table (Table_Page_Type,
                              Header_Type,
                              Contents_Type);

   type Table_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_Table_Page : Table_Pages.Table_Page;
      end record;

   Page_Slot_Count : constant Slot_Index := Table_Pages.Page_Slot_Count;

   function Last_Slot (Item : Table_Page_Handle'Class) return Slot_Index
   is (Table_Pages.Last_Slot (Item.The_Table_Page));

   function Get_Header (Table : Table_Page_Handle) return Header_Type
   is (Table_Pages.Get_Header (Table.The_Table_Page));

end Marlowe.Table_Page_Handles;
