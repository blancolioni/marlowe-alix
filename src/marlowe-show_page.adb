with Ada.Command_Line;                  use Ada.Command_Line;

with Marlowe.File_Handles;
with Marlowe.Btree_Page_Handles;
with Marlowe.Btree_Header_Page_Handles;

procedure Marlowe.Show_Page is
   
   use Marlowe.File_Handles;
   use Marlowe.Btree_Page_Handles;
   
   Handle       : File_Handle;
   Btree_Page   : Btree_Page_Handle;

   File : File_Index;
   Page : Page_Index;
   Loc  : File_And_Page;
   
   Btree_File : constant String := Argument (1);
   
begin
   
   Open (Handle, Btree_File);
   
   if Argument (2) = "root" then
      declare
	 Index : constant Positive := Positive'Value (Argument (3));
	 Header : Marlowe.Btree_Header_Page_Handles.Btree_Header_Page_Handle;
      begin
	 Read (Handle, Header, First_User_Page (Handle));
	 Loc := Btree_Header_Page_Handles.Get_Btree_Root (Header, Index);
      end;
   else
      File := File_Index'Value (Argument (2));
      Page := Page_Index'Value (Argument (3));
      Loc  := To_File_And_PAge (File, Page);
   end if;
   
   Read (Handle, Btree_Page, Loc); 
  
   Dump_Page (Btree_Page, True);
   
   Close (Handle);
   
end Marlowe.Show_Page;
