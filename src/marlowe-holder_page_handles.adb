package body Marlowe.Holder_Page_Handles is

   -------------
   -- Element --
   -------------

   function Element
     (Item   : in Holder_Page_Handle)
      return Element_Type
   is
   begin
      return Item.Get_Table_Value (1);
   end Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Item   : in Holder_Page_Handle;
      Value  : out Element_Type)
   is
   begin
      Item.Get_Table_Value (1, Value);
   end Read;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Item   : in Holder_Page_Handle;
      Element : Element_Type)
   is
   begin
      Item.Set_Table_Value (1, Element);
   end Replace_Element;

end Marlowe.Holder_Page_Handles;
