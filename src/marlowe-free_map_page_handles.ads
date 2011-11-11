with Marlowe.Pages.Free_Map;

with Marlowe.Page_Handles;

private package Marlowe.Free_Map_Page_Handles is

   type Free_Map_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle
     with private;

   procedure Set_Page (Handle    : in out Free_Map_Page_Handle;
                       Reference : in     File_And_Page);

   function Get_Free_Map_Page
     (Handle : Free_Map_Page_Handle)
     return Marlowe.Pages.Free_Map.Free_Map_Page;

   function Get_Free_Page
     (From  : Free_Map_Page_Handle)
     return Page_Index;

   procedure Free (In_Page : Free_Map_Page_Handle;
                   Index   : Page_Index);

   function Next_Free_Page (After : Free_Map_Page_Handle) return Page_Index;

private

   type Free_Map_Page_Handle is
     new Marlowe.Page_Handles.Page_Handle with
      record
         The_Free_Map_Page : Marlowe.Pages.Free_Map.Free_Map_Page;
      end record;

end Marlowe.Free_Map_Page_Handles;
