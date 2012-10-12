package body Marlowe.Btree.Data_Definition_Handles is

   ---------------------------
   -- Add_Record_Definition --
   ---------------------------

   function Add_Record_Definition
     (To_Page  : Data_Definition_Handle;
      Name     : String;
      Length   : System.Storage_Elements.Storage_Count)
      return Table_Index
   is
   begin
      return Marlowe.Pages.Data_Definition.Add_Record_Definition
        (To_Page.The_Data_Definition_Page, Name, Length);
   end Add_Record_Definition;

   ---------------------
   -- Allocate_Record --
   ---------------------

   function Allocate_Record (Item    : Data_Definition_Handle;
                             Index   : Table_Index)
                             return Database_Index
   is
      Result : constant Database_Index :=
                 Marlowe.Pages.Data_Definition.Allocate_Record
                   (Item.The_Data_Definition_Page,
                    Item.Local_Index (Index));
   begin
      Item.Set_Dirty;
      return Result;
   end Allocate_Record;

   ------------------------------
   -- Get_Data_Definition_Page --
   ------------------------------

   function Get_Data_Definition_Page
     (Handle : Data_Definition_Handle)
      return Marlowe.Pages.Data_Definition.Data_Definition_Page
   is
   begin
      return Handle.The_Data_Definition_Page;
   end Get_Data_Definition_Page;

   ------------------------------
   -- Get_Next_Record_Overflow --
   ------------------------------

   function Get_Next_Record_Overflow
     (Item  : Data_Definition_Handle;
      Index : Real_Table_Index)
      return File_Page_And_Slot
   is
   begin
      return Marlowe.Pages.Data_Definition.Get_Next_Record_Overflow
        (Item.The_Data_Definition_Page,
         Item.Local_Index (Index));
   end Get_Next_Record_Overflow;

   -----------------------
   -- Get_Overflow_Page --
   -----------------------

   function Get_Overflow_Page
     (From : Data_Definition_Handle)
      return File_And_Page
   is
   begin
      return Marlowe.Pages.Data_Definition.Get_Overflow_Page
        (From.The_Data_Definition_Page);
   end Get_Overflow_Page;

   -----------------------
   -- Get_Overflow_Page --
   -----------------------

   function Get_Overflow_Page
     (From : Data_Definition_Handle)
      return Data_Definition_Handle
   is
      Overflow : constant File_And_Page := Get_Overflow_Page (From);
      Result   : Data_Definition_Handle;
   begin
      Derive (From, Result);
      Set_Page (Result, Overflow);
      return Result;
   end Get_Overflow_Page;

   -----------------------
   -- Get_Record_Length --
   -----------------------

   function Get_Record_Length
     (Item     : in Data_Definition_Handle;
      Index    : in Table_Index)
      return System.Storage_Elements.Storage_Count
   is
   begin
      return Marlowe.Pages.Data_Definition.Get_Record_Length
        (Item.The_Data_Definition_Page, Item.Local_Index (Index));
   end Get_Record_Length;

   ---------------------
   -- Get_Record_Name --
   ---------------------

   function Get_Record_Name
     (Item  : Data_Definition_Handle;
      Index : Table_Index)
      return String
   is
   begin
      return Marlowe.Pages.Data_Definition.Get_Record_Name
        (Item.The_Data_Definition_Page, Item.Local_Index (Index));
   end Get_Record_Name;

   ---------------------
   -- Get_Record_Root --
   ---------------------

   function Get_Record_Root
     (Item  : Data_Definition_Handle;
      Index : Table_Index)
      return File_And_Page
   is
   begin
      return Marlowe.Pages.Data_Definition.Get_Record_Root
        (Item.The_Data_Definition_Page, Item.Local_Index (Index));
   end Get_Record_Root;

   -----------------------
   -- Last_Record_Index --
   -----------------------

   function Last_Record_Index (Item  : Data_Definition_Handle;
                               Table : Table_Index)
                              return Database_Index
   is
   begin
      return Marlowe.Pages.Data_Definition.Last_Record_Index
        (Item.The_Data_Definition_Page,
         Item.Local_Index (Table));
   end Last_Record_Index;

   -----------------
   -- Local_Index --
   -----------------

   function Local_Index
     (Handle : Data_Definition_Handle;
      Index  : Table_Index)
      return Marlowe.Pages.Data_Definition.Local_Record_Index
   is
   begin
      return
        Marlowe.Pages.Data_Definition.To_Local_Index
          (Handle.The_Data_Definition_Page,
           Index);
   end Local_Index;

   -------------------------------
   -- Maximum_Data_Record_Count --
   -------------------------------

   function Maximum_Data_Record_Count return Natural is
   begin
      return Marlowe.Pages.Data_Definition.Maximum_Data_Record_Count;
   end Maximum_Data_Record_Count;

   --------------
   -- New_Page --
   --------------

   procedure New_Page
     (Handle    : in out Data_Definition_Handle;
      Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.New_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Data_Definition_Page :=
        Marlowe.Pages.Data_Definition.To_Data_Definition_Page
          (Get_Page (Handle));
      Marlowe.Pages.Data_Definition.Initialise
        (Handle.The_Data_Definition_Page,
         Reference);
      Set_Dirty (Handle);
   end New_Page;

   ----------------------------
   -- Number_Of_Data_Records --
   ----------------------------

   function Number_Of_Data_Records
     (Item : Data_Definition_Handle)
      return Natural
   is
   begin
      return Marlowe.Pages.Data_Definition.Number_Of_Data_Records
        (Item.The_Data_Definition_Page);
   end Number_Of_Data_Records;

   ---------------------------
   -- Set_First_Table_Index --
   ---------------------------

   procedure Set_First_Table_Index (Item : Data_Definition_Handle;
                                    Index : Table_Index)
   is
   begin
      Item.Set_Dirty;
      Marlowe.Pages.Data_Definition.Set_First_Table_Index
        (Item.The_Data_Definition_Page, Index);
   end Set_First_Table_Index;

   ------------------------------
   -- Set_Next_Record_Overflow --
   ------------------------------

   procedure Set_Next_Record_Overflow
     (Item  : Data_Definition_Handle;
      Index : Real_Table_Index;
      Addr  : File_Page_And_Slot)
   is
   begin
      Item.Set_Dirty;
      Marlowe.Pages.Data_Definition.Set_Next_Record_Overflow
        (Item.The_Data_Definition_Page, Item.Local_Index (Index), Addr);
   end Set_Next_Record_Overflow;

   -----------------------
   -- Set_Overflow_Page --
   -----------------------

   procedure Set_Overflow_Page
     (Item          : Data_Definition_Handle;
      Overflow_Page : File_And_Page)
   is
   begin
      Item.Set_Dirty;
      Marlowe.Pages.Data_Definition.Set_Overflow_Page
        (Item.The_Data_Definition_Page,
         Overflow_Page);
   end Set_Overflow_Page;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page
     (Handle    : in out Data_Definition_Handle;
      Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.Set_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Data_Definition_Page :=
        Marlowe.Pages.Data_Definition.To_Data_Definition_Page
          (Get_Page (Handle));
   end Set_Page;

   ---------------------
   -- Set_Record_Root --
   ---------------------

   procedure Set_Record_Root
     (Item     : in Data_Definition_Handle;
      Index    : in Table_Index;
      New_Root : in File_And_Page)
   is
   begin
      Item.Set_Dirty;
      Marlowe.Pages.Data_Definition.Set_Record_Root
        (Item.The_Data_Definition_Page,
         Item.Local_Index (Index),
         New_Root);
   end Set_Record_Root;

end Marlowe.Btree.Data_Definition_Handles;
