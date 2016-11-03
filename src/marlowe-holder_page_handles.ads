--  private with Marlowe.Pages.Table;
private with Marlowe.Table_Page_Handles;

with Marlowe.Page_Handles;

generic
   Holder_Page_Type : Page_Type;
   type Element_Type is private;
package Marlowe.Holder_Page_Handles is

   type Holder_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   procedure Read (Item   : in Holder_Page_Handle;
                   Value  : out Element_Type);

   function Element (Item   : in Holder_Page_Handle)
                     return Element_Type;

   procedure Replace_Element
     (Item   : in Holder_Page_Handle;
      Element : Element_Type);

private

   type Holder_Page_Contents is mod 256;

--     package Holder_Pages is
--       new Marlowe.Pages.Table
--         (Table_Page_Type => Holder_Page_Type,
--          Header_Type     => Holder_Page_Contents,
--          Contents_Type   => Element_Type);

   package Holder_Handles is
     new Marlowe.Table_Page_Handles
       (Holder_Page_Type, Holder_Page_Contents, Element_Type);

   type Holder_Page_Handle is new Holder_Handles.Table_Page_Handle
   with null record;

end Marlowe.Holder_Page_Handles;
