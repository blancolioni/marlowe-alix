package body Marlowe.Free_Map_Page_Handles is

   ----------
   -- Free --
   ----------

   procedure Free (In_Page : Free_Map_Page_Handle;
                   Index   : Page_Index)
   is
   begin
      Set_Accessed (In_Page);
      Set_Dirty (In_Page);
      Marlowe.Pages.Free_Map.Free (In_Page.The_Free_Map_Page, Index);
   end Free;

   --------------------------
   -- Get_Free_Map_Page --
   --------------------------

   function Get_Free_Map_Page
     (Handle : Free_Map_Page_Handle)
     return Marlowe.Pages.Free_Map.Free_Map_Page
   is
   begin
      return Handle.The_Free_Map_Page;
   end Get_Free_Map_Page;

   -------------------
   -- Get_Free_Page --
   -------------------

   function Get_Free_Page
     (From  : Free_Map_Page_Handle)
     return Page_Index
   is
      Result : constant Page_Index :=
        Marlowe.Pages.Free_Map.Get_Free_Page (From.The_Free_Map_Page);
   begin
      Set_Accessed (From);
      if Result /= 0 then
         Set_Dirty (From);
      end if;
      return Result;
   end Get_Free_Page;

   --------------------
   -- Next_Free_Page --
   --------------------

   function Next_Free_Page (After : Free_Map_Page_Handle) return Page_Index is
   begin
      Set_Accessed (After);
      return Marlowe.Pages.Free_Map.Next_Free_Page (After.The_Free_Map_Page);
   end Next_Free_Page;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page (Handle    : in out Free_Map_Page_Handle;
                       Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.Set_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Free_Map_Page :=
        Marlowe.Pages.Free_Map.To_Free_Map_Page (Get_Page (Handle));
   end Set_Page;

end Marlowe.Free_Map_Page_Handles;
