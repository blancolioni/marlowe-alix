package body Marlowe.Btree_Header_Page_Handles is

   procedure Get_Overflow_Page (Item  : in out Btree_Header_Page_Handle;
                                Index : in out Positive);


   ---------------------------
   -- Add_Btree_Description --
   ---------------------------

   function Add_Btree_Description
     (To_Page    : Btree_Header_Page_Handle;
      Name       : String;
      Key_Length : System.Storage_Elements.Storage_Count)
     return Positive
   is
   begin
      Set_Dirty (To_Page);
      return Marlowe.Pages.Btree_Header.Add_Btree_Description
        (To_Page.The_Btree_Header_Page, Name, Key_Length);
   end Add_Btree_Description;

   ---------------------------
   -- Get_Btree_Header_Page --
   ---------------------------

   function Get_Btree_Header_Page
     (Handle : Btree_Header_Page_Handle)
     return Marlowe.Pages.Btree_Header.Btree_Header_Page
   is
   begin
      return Handle.The_Btree_Header_Page;
   end Get_Btree_Header_Page;

   --------------------------
   -- Get_Btree_Key_Length --
   --------------------------

   function Get_Btree_Key_Length (Item  : Btree_Header_Page_Handle;
                                  Index : Positive)
                                 return Positive
   is
      Overflow    : Btree_Header_Page_Handle := Item;
      Local_Index : Positive := Index;
   begin
      Get_Overflow_Page (Overflow, Local_Index);
      return Pages.Btree_Header.Get_Btree_Key_Length
        (Overflow.The_Btree_Header_Page, Local_Index);
   end Get_Btree_Key_Length;

   --------------------
   -- Get_Btree_Name --
   --------------------

   function Get_Btree_Name (Item  : Btree_Header_Page_Handle;
                            Index : Positive)
                           return String
   is
      Overflow    : Btree_Header_Page_Handle := Item;
      Local_Index : Positive := Index;
   begin
      Get_Overflow_Page (Overflow, Local_Index);
      return Pages.Btree_Header.Get_Btree_Name
        (Overflow.The_Btree_Header_Page, Local_Index);
   end Get_Btree_Name;

   --------------------
   -- Get_Btree_Root --
   --------------------

   function Get_Btree_Root (Item  : Btree_Header_Page_Handle;
                            Index : Positive)
                           return File_And_Page
   is
      Overflow    : Btree_Header_Page_Handle := Item;
      Local_Index : Positive := Index;
      Result      : File_And_Page;
   begin
      Get_Overflow_Page (Overflow, Local_Index);
      Result := Pages.Btree_Header.Get_Btree_Root
        (Overflow.The_Btree_Header_Page, Local_Index);
      return Result;
   end Get_Btree_Root;

   -------------------------
   -- Get_Key_Table_Index --
   -------------------------

   function Get_Key_Table_Index (Item  : in Btree_Header_Page_Handle;
                                 Index : in Positive)
                                 return Table_Index
   is
      Overflow    : Btree_Header_Page_Handle := Item;
      Local_Index : Positive := Index;
   begin
      Get_Overflow_Page (Overflow, Local_Index);
      return Pages.Btree_Header.Get_Key_Table_Index
        (Overflow.The_Btree_Header_Page, Local_Index);
   end Get_Key_Table_Index;

   -----------------------------
   -- Get_Leaf_Minimum_Degree --
   -----------------------------

   function Get_Leaf_Minimum_Degree (Item  : Btree_Header_Page_Handle;
                                     Index : Positive)
                                    return Natural
   is
      Overflow    : Btree_Header_Page_Handle := Item;
      Local_Index : Positive := Index;
   begin
      Get_Overflow_Page (Overflow, Local_Index);
      return Pages.Btree_Header.Get_Leaf_Minimum_Degree
        (Overflow.The_Btree_Header_Page, Local_Index);
   end Get_Leaf_Minimum_Degree;

   ---------------
   -- Get_Magic --
   ---------------

   function Get_Magic
     (Item : Btree_Header_Page_Handle)
      return Marlowe_Magic_Number
   is
   begin
      return Marlowe.Pages.Btree_Header.Get_Magic
        (Item.The_Btree_Header_Page);
   end Get_Magic;

   -----------------------------
   -- Get_Node_Minimum_Degree --
   -----------------------------

   function Get_Node_Minimum_Degree (Item  : Btree_Header_Page_Handle;
                                     Index : Positive)
                                    return Natural
   is
      Overflow    : Btree_Header_Page_Handle := Item;
      Local_Index : Positive := Index;
   begin
      Get_Overflow_Page (Overflow, Local_Index);
      return Pages.Btree_Header.Get_Node_Minimum_Degree
        (Overflow.The_Btree_Header_Page, Local_Index);
   end Get_Node_Minimum_Degree;

   -----------------------
   -- Get_Overflow_Page --
   -----------------------

   function Get_Overflow_Page (From : Btree_Header_Page_Handle)
                              return File_And_Page
   is
   begin
      return Pages.Btree_Header.Get_Overflow_Page (From.The_Btree_Header_Page);
   end Get_Overflow_Page;

   -----------------------
   -- Get_Overflow_Page --
   -----------------------

   function Get_Overflow_Page (From : Btree_Header_Page_Handle)
                              return Btree_Header_Page_Handle
   is
      Overflow : constant File_And_Page := Get_Overflow_Page (From);
      Result   : Btree_Header_Page_Handle;
   begin
      Derive (From, Result);
      Set_Page (Result, Overflow);
      return Result;
   end Get_Overflow_Page;

   -----------------------
   -- Get_Overflow_Page --
   -----------------------

   procedure Get_Overflow_Page (Item  : in out Btree_Header_Page_Handle;
                                Index : in out Positive)
   is
   begin
      while Index > Maximum_Btree_Count loop
         Item := Get_Overflow_Page (Item);
         Index := Index - Maximum_Btree_Count;
      end loop;
   end Get_Overflow_Page;

   --------------------
   -- Get_Table_Page --
   --------------------

   function Get_Table_Page (Item : in Btree_Header_Page_Handle)
                            return File_And_Page
   is
   begin
      return Marlowe.Pages.Btree_Header.Get_Table_Page
        (Item.The_Btree_Header_Page);
   end Get_Table_Page;

   ----------------------
   -- Get_Total_Btrees --
   ----------------------

   function Get_Total_Btrees (For_Page : Btree_Header_Page_Handle)
                             return Natural
   is
   begin
      return Pages.Btree_Header.Get_Total_Btrees
        (For_Page.The_Btree_Header_Page);
   end Get_Total_Btrees;

   ----------------------------
   -- Increment_Total_Btrees --
   ----------------------------

   procedure Increment_Total_Btrees (For_Page : Btree_Header_Page_Handle) is
   begin
      Set_Dirty (For_Page);
      Pages.Btree_Header.Increment_Total_Btrees
        (For_Page.The_Btree_Header_Page);
   end Increment_Total_Btrees;

   -------------------------
   -- Maximum_Btree_Count --
   -------------------------

   function Maximum_Btree_Count return Natural is
   begin
      return Marlowe.Pages.Btree_Header.Maximum_Btree_Count;
   end Maximum_Btree_Count;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (Handle    : in out Btree_Header_Page_Handle;
                       Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.New_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Btree_Header_Page :=
        Marlowe.Pages.Btree_Header.To_Btree_Header_Page (Get_Page (Handle));
      Marlowe.Pages.Btree_Header.Initialise (Handle.The_Btree_Header_Page,
                                             Reference);
      Set_Dirty (Handle);
   end New_Page;

   ----------------------
   -- Number_Of_Btrees --
   ----------------------

   function Number_Of_Btrees
     (Item : Btree_Header_Page_Handle)
     return Natural
   is
   begin
      return Pages.Btree_Header.Number_Of_Btrees (Item.The_Btree_Header_Page);
   end Number_Of_Btrees;

   --------------------
   -- Set_Btree_Root --
   --------------------

   procedure Set_Btree_Root (Item     : Btree_Header_Page_Handle;
                             Index    : Positive;
                             New_Root : File_And_Page)
   is
      Overflow    : Btree_Header_Page_Handle := Item;
      Local_Index : Positive := Index;
   begin
      Get_Overflow_Page (Overflow, Local_Index);
      Pages.Btree_Header.Set_Btree_Root
        (Overflow.The_Btree_Header_Page, Local_Index, New_Root);
      Set_Dirty (Overflow);
   end Set_Btree_Root;

   ---------------
   -- Set_Magic --
   ---------------

   procedure Set_Magic (Item  : Btree_Header_Page_Handle;
                        Magic : Marlowe_Magic_Number)
   is
   begin
      Marlowe.Pages.Btree_Header.Set_Magic (Item.The_Btree_Header_Page,
                                            Magic);
   end Set_Magic;

   -----------------------
   -- Set_Overflow_Page --
   -----------------------

   procedure Set_Overflow_Page (Item          : Btree_Header_Page_Handle;
                                Overflow_Page : File_And_Page)
   is
   begin
      Set_Dirty (Item);
      Pages.Btree_Header.Set_Overflow_Page (Item.The_Btree_Header_Page,
                                            Overflow_Page);
   end Set_Overflow_Page;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page (Handle    : in out Btree_Header_Page_Handle;
                       Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.Set_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Btree_Header_Page :=
        Marlowe.Pages.Btree_Header.To_Btree_Header_Page (Get_Page (Handle));
   end Set_Page;

   --------------------
   -- Set_Table_Page --
   --------------------

   procedure Set_Table_Page (Item : in Btree_Header_Page_Handle;
                             Loc  : in File_And_Page)
   is
   begin
      Marlowe.Pages.Btree_Header.Set_Table_Page
        (Item.The_Btree_Header_Page, Loc);
   end Set_Table_Page;

end Marlowe.Btree_Header_Page_Handles;
