with Marlowe.Btree;

generic
   type Element_Type is private;
   Null_Element : Element_Type;
   with function Get_Page_Index (From : Element_Type)
                                return Marlowe.Btree.Page_Index;
package Marlowe.Btree.Cache is

   procedure Insert_Cache_Entry (Element : Element_Type);
   procedure Remove_Cache_Entry (Index : Page_Index);
   function  Get_Cache_Entry (Index : Page_Index) return Element_Type;

   procedure Close;

   type Action is
     access procedure (Element : in Element_Type);

   procedure For_All_Cache_Entries (Do_This : Action);

end Marlowe.Btree.Cache;


