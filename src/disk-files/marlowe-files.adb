with Ada.Direct_IO;
with Ada.Unchecked_Deallocation;

with Marlowe.Allocation;

package body Marlowe.Files is

   package File_Of_Pages is
      new Ada.Direct_IO (Marlowe.Pages.Page_Record);

   Max_Pages_In_File : constant := 2 ** 24;

   protected type Marlowe_File is
      procedure Read (From : in     Page_Index;
                      Item :    out Pages.Page_Record);

      procedure Write (Item : in Pages.Page_Record);

      function New_Page return Page_Index;

      function Max_Page_Index return Page_Index;
      function Read_Count return Natural;
      function Write_Count return Natural;
      procedure Clear (Page : Page_Index);
      procedure Open (Name : String);
      procedure Create (Name : String);
      procedure Close;

      function Is_Open return Boolean;

   private
      File          : File_Of_Pages.File_Type;
      Open_Flag     : Boolean := False;
      Reads, Writes : Natural := 0;
   end Marlowe_File;

   type Array_Of_Marlowe_Files is
     array (File_Index range 1 .. Max_Files) of Marlowe_File;

   type File_Type_Record is
      record
         Files      : Array_Of_Marlowe_Files;
      end record;

   -----------
   -- Clear --
   -----------

   procedure Clear (File     : in File_Type;
                    Location : in File_And_Page)
   is
      File_Id : constant File_Index := Get_File (Location);
      Page_Id : constant Page_Index := Get_Page (Location);
   begin
      File.Files (File_Id).Clear (Page_Id);
   end Clear;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
      procedure Free is
         new Ada.Unchecked_Deallocation (File_Type_Record, File_Type);
   begin
      for I in File.Files'Range loop
         if File.Files (I).Is_Open then
            File.Files (I).Close;
         end if;
      end loop;
      Free (File);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File  : in out File_Type;
                     Index : in     File_Index;
                     Name  : in     String)
   is
   begin

      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      pragma Assert (Index > 1 or else File = null);
      pragma Assert (Index = 1 or else File /= null);

      if File = null then
         File := new File_Type_Record;
         if Marlowe.Allocation.Debug_Allocation then
            Marlowe.Allocation.Allocate (File.all'Size, "file-type");
         end if;
      end if;

      File.Files (Index).Create (Name);

   end Create;

   --------------------
   -- Get_Statistics --
   --------------------

   procedure Get_Statistics
     (File   : File_Type;
      Last   : out Page_Index;
      Reads  : out Natural;
      Writes : out Natural)
   is
   begin
      Reads := 0;
      Writes := 0;
      Last   := 0;
      for F of File.Files loop
         Reads := Reads + F.Read_Count;
         Writes := Writes + F.Write_Count;
         Last := Page_Index'Max (F.Max_Page_Index, Last);
      end loop;
   end Get_Statistics;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File  : File_Type;
                     Index : File_Index)
                    return Boolean
   is
   begin
      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      return File.Files (Index).Is_Open;
   end Is_Open;

   ------------------
   -- Marlowe_File --
   ------------------

   protected body Marlowe_File is

      -----------
      -- Clear --
      -----------

      procedure Clear (Page : Page_Index) is
         use File_Of_Pages;
         Data    : Marlowe.Pages.Page_Record;
         pragma Warnings (Off, Data);
         Page_Id : constant Positive_Count := Positive_Count (Page);
      begin

         Set_Index (File, Page_Id);
         Write (File, Data);

         pragma Assert (Size (File) >= Page_Id);

         --  We don't actually clear it, for no particular reason.
         --  This function just ensures that the given location
         --  exists, by extending the file if necessary.
         --  Clearing should not be necessary, since it will be
         --  overwritten with new data soon enough.
      end Clear;

      -----------
      -- Close --
      -----------

      procedure Close is
      begin
         File_Of_Pages.Close (File);
      end Close;

      ------------
      -- Create --
      ------------

      procedure Create (Name : String) is
         use File_Of_Pages;
      begin
         if not Open_Flag then
            Create (File, Inout_File, Name);
            Open_Flag := True;
         end if;
      end Create;

      -------------
      -- Is_Open --
      -------------

      function Is_Open return Boolean is
      begin
         return Open_Flag;
      end Is_Open;

      --------------------
      -- Max_Page_Index --
      --------------------

      function Max_Page_Index return Page_Index is
      begin
         if Open_Flag then
            return Page_Index (File_Of_Pages.Size (File));
         else
            return 0;
         end if;
      end Max_Page_Index;

      --------------
      -- New_Page --
      --------------

      function New_Page return Page_Index
      is
         use File_Of_Pages;
      begin
         pragma Assert (Open_Flag);

         if Page_Index (Size (File)) >= Max_Pages_In_File then
            return 0;
         else
            declare
               Current_Size : constant Count := Size (File);
               Data         : Marlowe.Pages.Page_Record;
               pragma Warnings (Off, Data);
               New_Size     : Count;
            begin
               Set_Index (File, Current_Size + 1);
               Write (File, Data);
               New_Size := Size (File);
               return Page_Index (New_Size);
            end;
         end if;
      end New_Page;

      ----------
      -- Open --
      ----------

      procedure Open (Name : String) is
         use File_Of_Pages;
      begin
         if not Open_Flag then
            Open (File, Inout_File, Name);
            Open_Flag := True;
         end if;
      end Open;

      ----------
      -- Read --
      ----------

      procedure Read (From : in     Page_Index;
                      Item :    out Pages.Page_Record)
      is
      begin
         pragma Assert (From <=
                          Page_Index (File_Of_Pages.Size (File)));
         File_Of_Pages.Read (File, Item, File_Of_Pages.Positive_Count (From));
         Reads := Reads + 1;
      end Read;

      ----------------
      -- Read_Count --
      ----------------

      function Read_Count return Natural is
      begin
         return Reads;
      end Read_Count;

      -----------
      -- Write --
      -----------

      procedure Write (Item : in Pages.Page_Record) is
         Location       : constant File_And_Page :=
                            Marlowe.Pages.Get_File_And_Page (Item);
         Last_File_Page : constant Page_Index :=
                            Page_Index (File_Of_Pages.Size (File));
      begin
         pragma Assert (Get_Page (Location) <= Last_File_Page + 1);

         File_Of_Pages.Write (File,
                              Item,
                              File_Of_Pages.Positive_Count
                                (Get_Page (Location)));
         declare
            New_Last_Page : constant Page_Index :=
                              Page_Index (File_Of_Pages.Size (File));
         begin
            pragma Assert (New_Last_Page = Last_File_Page or else
                             (New_Last_Page = Last_File_Page + 1
                 and then Get_Page (Location) = New_Last_Page));
         end;

         Writes := Writes + 1;

      end Write;

      -----------------
      -- Write_Count --
      -----------------

      function Write_Count return Natural is
      begin
         return Writes;
      end Write_Count;

   end Marlowe_File;

   --------------------
   -- Max_Page_Index --
   --------------------

   function Max_Page_Index (File  : File_Type;
                            Index : File_Index)
                           return Page_Index
   is
   begin
      return File.Files (Index).Max_Page_Index;
   end Max_Page_Index;

   --------------
   -- New_Page --
   --------------

   function New_Page (File  : File_Type;
                      Index : File_Index)
                     return Page_Index
   is
   begin
      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);

      return File.Files (Index).New_Page;
   end New_Page;

   ----------
   -- Open --
   ----------

   procedure Open (File : in out File_Type;
                   Index : in     File_Index;
                   Name : in     String)
   is
   begin

      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      pragma Assert (Index > 1 or else File = null);
      pragma Assert (Index = 1 or else File /= null);

      if File = null then
         File := new File_Type_Record;
         if Marlowe.Allocation.Debug_Allocation then
            Marlowe.Allocation.Allocate (File.all'Size, "file-type");
         end if;
      end if;

      File.Files (Index).Open (Name);

   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File : in     File_Type;
                   From : in     File_And_Page;
                   Item :    out Pages.Page_Record)
   is
   begin
      pragma Assert (Get_File (From) > 0);
      pragma Assert (Get_File (From) <= Max_Files);
      --  pragma Assert (File.Open (Get_File (From)));
      pragma Assert (Get_Page (From) <= Max_Pages_In_File);
      pragma Assert (Get_Page (From) > 0);
      pragma Assert (File /= null);

      File.Files (Get_File (From)).Read (Get_Page (From), Item);
      pragma Assert (Pages.Validate (Item));
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (File : in File_Type;
                    Item : in Pages.Page_Record)
   is
      Location       : constant File_And_Page :=
                         Marlowe.Pages.Get_File_And_Page (Item);
   begin
      pragma Assert (Get_File (Location) > 0);
      pragma Assert (Get_File (Location) <= Max_Files);
      pragma Assert (Get_Page (Location) <= Max_Pages_In_File);
      pragma Assert (Get_Page (Location) > 0);
      pragma Assert (File /= null);

      File.Files (Get_File (Location)).Write (Item);
   end Write;

end Marlowe.Files;
