with Ada.Direct_IO;
with Ada.Unchecked_Deallocation;

with Marlowe.Trace;

package body Marlowe.Btree.Disk_Pages is

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
      Marlowe.Trace.Put_Line ("Size of page: " &
                         Btree_Disk_Page'Size'Img);
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (From_Page : in out Btree_Disk_Page;
                     Index     : in     Child_Index)
   is
   begin
      pragma Assert (not From_Page.Leaf or else Index <= From_Page.Num_Keys);

      if Index <= From_Page.Num_Keys then
         for I in Index .. From_Page.Num_Keys - 1 loop
            From_Page.Keys (I)     := From_Page.Keys (I + 1);
            From_Page.Children (I) := From_Page.Children (I + 1);
         end loop;
         From_Page.Children (From_Page.Num_Keys) :=
           From_Page.Children (From_Page.Num_Keys + 1);
      end if;

      From_Page.Num_Keys := From_Page.Num_Keys - 1;

   end Delete;

   --------------
   -- Find_Key --
   --------------

   function Find_Key
     (In_Page : Btree_Disk_Page;
      Key     : Key_Type;
      Forward : Boolean     := True)
      return Child_Index
   is
      Result : Child_Index;
   begin
      if Forward then
         Result := 1;
         while Result <= In_Page.Num_Keys loop
            if In_Page.Keys (Result) = Key then
               return Result;
            elsif Key < In_Page.Keys (Result) then
               return Result;
            else
               Result := Result + 1;
            end if;
         end loop;
      else
         Result := In_Page.Num_Keys + 1;
         while Result > 1 loop
            if In_Page.Keys (Result - 1) = Key then
               return Result - 1;
            elsif In_Page.Keys (Result - 1) < Key then
               return Result;
            else
               Result := Result - 1;
            end if;
         end loop;
      end if;
      return Result;
   end Find_Key;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (From_Page : Btree_Disk_Page;
      Index     : Child_Index)
      return Page_Index
   is
   begin
      pragma Assert (Index <= From_Page.Num_Keys + 1);
      --  pragma Assert (not From_Page.Leaf);
      if From_Page.Leaf then
         return 0;
      else
         return From_Page.Children (Index);
      end if;
   end Get_Child;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index  (Of_Page : Btree_Disk_Page) return Page_Index is
   begin
      return Of_Page.Index;
   end Get_Index;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (From_Page : Btree_Disk_Page;
      Index     : Key_Index)
      return Key_Type
   is
   begin
      pragma Assert (Index <= From_Page.Num_Keys);
      return From_Page.Keys (Index);
   end Get_Key;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Of_Page : Btree_Disk_Page) return Page_Index is
   begin
      return Of_Page.Parent;
   end Get_Parent;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Page   : in out Btree_Disk_Page;
      Index  : in     Page_Index;
      Leaf   : in     Boolean)
   is
   begin
      Page.Leaf  := Leaf;
      Page.Index := Index;
   end Initialise;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (To_Page : in out Btree_Disk_Page;
      Key     : in     Key_Type;
      Child   : in     Page_Index)
   is
      Index : Key_Index;
   begin
      pragma Assert (To_Page.Num_Keys < Max_Keys_In_Page);

      if not To_Page.Leaf then
         To_Page.Children (To_Page.Num_Keys + 2) :=
           To_Page.Children (To_Page.Num_Keys + 1);
      end if;

      To_Page.Num_Keys := To_Page.Num_Keys + 1;
      Index := To_Page.Num_Keys;
      while Index > 1 and then
        Key < To_Page.Keys (Index - 1)
      loop
         To_Page.Keys (Index) := To_Page.Keys (Index - 1);
         To_Page.Children (Index) := To_Page.Children (Index - 1);
         Index := Index - 1;
      end loop;

      To_Page.Keys (Index) := Key;
      To_Page.Children (Index) := Child;

   end Insert;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Page : Btree_Disk_Page) return Boolean is
   begin
      return Page.Leaf;
   end Is_Leaf;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Page : Btree_Disk_Page) return Boolean is
   begin
      return Page.Parent = Null_Page_Index;
   end Is_Root;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (Page : out Disk_Pages.Btree_Disk_Page) is
      use File_Of_Pages;
      Index : constant Positive_Count := Size (Current.File) + 1;
   begin
      Disk_Pages.Initialise (Page, Page_Index (Index), True);
      Write (Current.File, Page, Index);
   end New_Page;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys (In_Page : Btree_Disk_Page) return Key_Count is
   begin
      return In_Page.Num_Keys;
   end Number_Of_Keys;

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
      --  Dump_Page (Page);
   end Read_Page;

   ---------------
   -- Root_Page --
   ---------------

   function Root_Page return Page_Index is
   begin
      return Current.Info.Root_Page;
   end Root_Page;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (In_Page : in out Btree_Disk_Page;
                        Index   : in     Child_Index;
                        Child   : in     Page_Index)
   is
   begin
      In_Page.Children (Index) := Child;
   end Set_Child;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (In_Page : in out Btree_Disk_Page;
                      Index   : in     Key_Index;
                      Key     : in     Key_Type)
   is
   begin
      In_Page.Keys (Index) := Key;
   end Set_Key;

   --------------
   -- Set_Leaf --
   --------------

   procedure Set_Leaf (Page : in out Btree_Disk_Page;
                       Leaf : in     Boolean)
   is
   begin
      Page.Leaf := Leaf;
   end Set_Leaf;

   ------------------------
   -- Set_Number_Of_Keys --
   ------------------------

   procedure Set_Number_Of_Keys (In_Page : in out Btree_Disk_Page;
                                 Num     : in     Key_Count)
   is
   begin
      In_Page.Num_Keys := Num;
   end Set_Number_Of_Keys;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (Of_Page : in out Btree_Disk_Page;
                         Parent  : in     Page_Index)
   is
   begin
      Of_Page.Parent := Parent;
   end Set_Parent;

   ---------------
   -- Set_Root --
   --------------

   procedure Set_Root (Page  : in     Page_Index) is
   begin
      Current.Info.Root_Page := Page;
   end Set_Root;

   -----------
   -- Split --
   -----------

   procedure Split
     (Source   : in out Btree_Disk_Page;
      New_Page : in out Btree_Disk_Page;
      Index    : in     Key_Index;
      Parent   : in out Btree_Disk_Page)
   is
   begin
      pragma Assert (Source.Num_Keys = Max_Keys_In_Page);
      pragma Assert (New_Page.Num_Keys = 0);
      pragma Assert (New_Page.Index /= Null_Page_Index);
      pragma Assert (Parent.Num_Keys < Max_Keys_In_Page);

      New_Page.Leaf := Source.Leaf;
      New_Page.Num_Keys := Minimum_Degree - 1;

      for I in 1 .. Minimum_Degree - 1 loop
         New_Page.Keys (I) := Source.Keys (I + Minimum_Degree);
      end loop;

      if not New_Page.Leaf then
         for I in 1 .. Minimum_Degree loop
            New_Page.Children (I) := Source.Children (I + Minimum_Degree);
         end loop;
      end if;

      Source.Num_Keys := Minimum_Degree - 1;
      for I in reverse Index + 1 .. Parent.Num_Keys + 1 loop
         Parent.Children (I + 1) := Parent.Children (I);
      end loop;
      Parent.Children (Index + 1) := New_Page.Index;

      for I in reverse Index .. Parent.Num_Keys loop
         Parent.Keys (I + 1) := Parent.Keys (I);
      end loop;
      Parent.Keys (Index) := Source.Keys (Minimum_Degree);
      Parent.Num_Keys := Parent.Num_Keys + 1;
      New_Page.Parent := Parent.Index;
   end Split;

   ----------------
   -- Write_Page --
   ----------------

   procedure Write_Page (Page   : in     Disk_Pages.Btree_Disk_Page) is
   begin
      File_Of_Pages.Write (Current.File, Page,
                           File_Of_Pages.Positive_Count
                           (Disk_Pages.Get_Index (Page)));
   end Write_Page;

   ---------------
   -- Dump_Page --
   ---------------

   procedure Dump_Page (Page : in Btree_Disk_Page) is
   begin
      Marlowe.Trace.Put_Line ("Index =" & Page.Index'Img);
      Marlowe.Trace.Put_Line ("Leaf  = " & Page.Leaf'Img);
      Marlowe.Trace.Put_Line ("Root  = " & Boolean'Image (Is_Root (Page)));
      Marlowe.Trace.Put_Line ("Parent =" & Page.Parent'Img);
      Marlowe.Trace.Put_Line ("Num_Keys =" & Page.Num_Keys'Img);
      if not Page.Leaf then
         for I in 1 .. Page.Num_Keys + 1 loop
            Marlowe.Trace.Put_Line ("    " & I'Img &
                                    "; child =" & Page.Children (I)'Img);
         end loop;
      end if;
   end Dump_Page;

end Marlowe.Btree.Disk_Pages;
