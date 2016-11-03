package body Marlowe.Table_Page_Handles is

   --------------------
   -- Get_Table_Page --
   --------------------

--     function Get_Table_Page
--       (Handle : Table_Page_Handle)
--       return Table_Pages.Table_Page
--     is
--     begin
--        return Handle.The_Table_Page;
--     end Get_Table_Page;

   ---------------------
   -- Get_Table_Value --
   ---------------------

   function Get_Table_Value (Item   : in Table_Page_Handle;
                             Offset : Slot_Index)
                             return Contents_Type
   is
   begin
      return Table_Pages.Get_Table_Value (Item.The_Table_Page, Offset);
   end Get_Table_Value;

   ---------------------
   -- Get_Table_Value --
   ---------------------

   procedure Get_Table_Value (Item   : in Table_Page_Handle;
                              Offset : Slot_Index;
                              Value  : out Contents_Type)
   is
   begin
      Value := Table_Pages.Get_Table_Value (Item.The_Table_Page, Offset);
   end Get_Table_Value;

   --------------
   -- New_Page --
   --------------

   overriding procedure New_Page
     (Handle    : in out Table_Page_Handle;
      Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.New_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Table_Page :=
        Table_Pages.To_Table_Page (Get_Page (Handle));
      Table_Pages.Initialise (Handle.The_Table_Page,
                              Reference);
      Set_Dirty (Handle);
   end New_Page;

   ----------------
   -- Scan_Table --
   ----------------

   procedure Scan_Table
     (Item    : Table_Page_Handle'Class;
      Process : not null access
        procedure (Slot : Slot_Index;
                   Value : Contents_Type))
   is
   begin
      for I in 1 .. Table_Pages.Last_Slot (Item.The_Table_Page) loop
         Process (I,
                  Table_Pages.Get_Table_Value
                  (Item.The_Table_Page, I));
      end loop;
   end Scan_Table;

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header (Table  : Table_Page_Handle;
                         Header : Header_Type)
   is
   begin
      Table_Pages.Set_Header (Table.The_Table_Page, Header);
   end Set_Header;

   --------------
   -- Set_Page --
   --------------

   overriding procedure Set_Page (Handle    : in out Table_Page_Handle;
                       Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.Set_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Table_Page :=
        Table_Pages.To_Table_Page (Get_Page (Handle));
   end Set_Page;

   ---------------------
   -- Set_Table_Value --
   ---------------------

   procedure Set_Table_Value (Item   : in Table_Page_Handle;
                              Offset : Slot_Index;
                              Value  : Contents_Type)
   is
   begin
      Item.Set_Dirty;
      Table_Pages.Set_Table_Value (Item.The_Table_Page, Offset, Value);
   end Set_Table_Value;

end Marlowe.Table_Page_Handles;
