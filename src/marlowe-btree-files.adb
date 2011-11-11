with Ada.Direct_IO;
with Ada.Unchecked_Deallocation;

with Marlowe.Trace;

package body Marlowe.Btree.Files is

   type File_Name is access String;

   type File_Info_Record is
      record
         Root_Page     : Page_Index := 1;
      end record;

   package File_Of_Pages is
      new Ada.Direct_IO (Disk_Pages.Btree_Disk_Page);

   package File_Of_Info_Records is
      new Ada.Direct_IO (File_Info_Record);

   type Btree_File_Record is
      record
         Name      : File_Name;
         Info      : File_Info_Record;
         File      : File_Of_Pages.File_Type;
      end record;

   Current : Btree_File_Record;

   -----------
   -- Close --
   -----------

   procedure Close is
      procedure Free is
         new Ada.Unchecked_Deallocation (String, File_Name);
   begin

      declare
         Info_File : File_Of_Info_Records.File_Type;
      begin
         File_Of_Info_Records.Open (Info_File,
                                    File_Of_Info_Records.Inout_File,
                                    Current.Name.all & ".info");
         File_Of_Info_Records.Write (Info_File, Current.Info, 1);
         File_Of_Info_Records.Close (Info_File);
      end;


      File_Of_Pages.Close (Current.File);
      Free (Current.Name);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (Name : in     String)
   is
   begin
      declare
         Info_File : File_Of_Info_Records.File_Type;
         Info      : File_Info_Record;
      begin
         File_Of_Info_Records.Create (Info_File,
                                      File_Of_Info_Records.Out_File,
                                      Name & ".info");
         File_Of_Info_Records.Write (Info_File, Info, 1);
         File_Of_Info_Records.Close (Info_File);
      end;

      File_Of_Pages.Create (Current.File, File_Of_Pages.Inout_File,
                            Name & ".key");
      Current.Name := new String'(Name);

      declare
         Root : Disk_Pages.Btree_Disk_Page;
      begin
         Disk_Pages.Initialise (Root, 1, True);
         Write_Page (Root);
      end;

      Marlowe.Trace.Put_Line ("Creating: " & Name);
      Marlowe.Trace.Put_Line ("Min keys in page: " &
                         Min_Keys_In_Page'Img);
      Marlowe.Trace.Put_Line ("Max keys in page: " &
                         Disk_Pages.Max_Keys_In_Page'Img);

   end Create;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (Page : out Disk_Pages.Btree_Disk_Page) is
      use File_Of_Pages;
      Index : Positive_Count := Size (Current.File) + 1;
   begin
      Disk_Pages.Initialise (Page, Page_Index (Index), True);
      Write (Current.File, Page, Index);
   end New_Page;

   ----------
   -- Open --
   ----------

   procedure Open (Name : in     String) is
   begin
      declare
         Info_File : File_Of_Info_Records.File_Type;
      begin
         File_Of_Info_Records.Open (Info_File,
                                    File_Of_Info_Records.In_File,
                                    Name & ".info");
         File_Of_Info_Records.Read (Info_File, Current.Info, 1);
         File_Of_Info_Records.Close (Info_File);
      end;

      Current.Name := new String'(Name);
      File_Of_Pages.Open (Current.File, File_Of_Pages.Inout_File,
                            Name & ".key");

   end Open;

   ---------------
   -- Read_Page --
   ---------------

   procedure Read_Page (Index   : in     Page_Index;
                        Page    :    out Disk_Pages.Btree_Disk_Page)
   is
   begin
      File_Of_Pages.Read (Current.File,
                          Page,
                          File_Of_Pages.Positive_Count (Index));
   end Read_Page;

   ---------------
   -- Root_Page --
   ---------------

   function Root_Page return Page_Index is
   begin
      return Current.Info.Root_Page;
   end Root_Page;

   ---------------
   -- Set_Root --
   --------------

   procedure Set_Root (Page  : in     Page_Index) is
   begin
      Current.Info.Root_Page := Page;
   end Set_Root;

   ----------------
   -- Write_Page --
   ----------------

   procedure Write_Page (Page   : in     Disk_Pages.Btree_Disk_Page) is
   begin
      File_Of_Pages.Write (Current.File, Page,
                           File_Of_Pages.Positive_Count
                           (Disk_Pages.Get_Index (Page)));
   end Write_Page;

end Marlowe.Btree.Files;

