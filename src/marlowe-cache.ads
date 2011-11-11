with Marlowe.Pages;

private generic
   type Element_Type is private;
   Null_Element : Element_Type;
   with function Get_File_And_Page (From : Element_Type)
				   return File_And_Page;
package Marlowe.Cache is

   procedure Insert_Cache_Entry (Element : Element_Type);
   procedure Remove_Cache_Entry (Index : Page_Index);
   function  Get_Cache_Entry (Index : Page_Index) return Element_Type;

   procedure Close;

   type Action is
     access procedure (Element : in Element_Type);

   procedure For_All_Cache_Entries (Do_This : Action);

end Marlowe.Cache;


