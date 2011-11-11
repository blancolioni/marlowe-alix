with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Marlowe.Allocation;
with Marlowe.Caches;
with Marlowe.File_Header_Page_Handles;
with Marlowe.Files;
with Marlowe.Free_Map_Page_Handles;
with Marlowe.Locks;

with Marlowe.Pages.File_Header;
with Marlowe.Pages.Free_Map;

package body Marlowe.File_Handles is

   Cache_Size : constant := 2 ** 18;

   Header_Page_Index     : constant Page_Index := 1;
   pragma Unreferenced (Header_Page_Index);  --  FIXME: why?

   First_Free_Map_Index  : constant Page_Index := 2;
   First_User_Page_Index : constant Page_Index := 3;

   type File_Info_Record is
      record
         Header : Marlowe.File_Header_Page_Handles.File_Header_Page_Handle;
      end record;

   type File_Info is
     array (File_Index range 1 .. Files.Max_Files) of File_Info_Record;

   type File_Handle_Record is
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         File      : Marlowe.Files.File_Type;
         Cache     : Marlowe.Caches.File_Cache;
         Info      : File_Info;
         File_Lock : Marlowe.Locks.Lock;
      end record;

   procedure Create_File (File  : in File_Handle;
                          Index : in File_Index);

   function Get_File_Name (File  : in File_Handle;
                           Index : in File_Index)
                          return String;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (File : in     File_Handle;
                       Page : in out Marlowe.Page_Handles.Page_Handle'Class)
   is

      use File_Header_Page_Handles;
      use Free_Map_Page_Handles;

      Current_File   : File_Index            := 1;
      New_Page       : Page_Index            := 0;
      Free_Map       : Free_Map_Page_Handle;

   begin

      Locks.Exclusive_Lock (File.File_Lock);

      Set_Cache (Free_Map, File.Cache);
      Marlowe.Page_Handles.Set_Cache (Page, File.Cache);

      Loop_Over_Files :
      loop

         if not Files.Is_Open (File.File, Current_File) then
            Files.Open (File.File, Current_File,
                        Get_File_Name (File, Current_File));
         end if;

         declare
            Free_Map_Index : Page_Index := First_Free_Map_Index;
            Max_Page_Index : constant Page_Index :=
              Marlowe.Files.Max_Page_Index (File.File, Current_File);
         begin

            Loop_Over_Free_Maps :
            loop

               if Free_Map_Index > Max_Page_Index then
                  declare
                     use Marlowe.Pages.Free_Map;
                     New_Map : Free_Map_Page := new Free_Map_Page_Record;
                  begin
                     Initialise (New_Map, To_File_And_Page (Current_File,
                                                            Free_Map_Index));
                     Files.Write (File.File, To_Page (New_Map).all);
                     Release (New_Map);
                  end;

               end if;

               Set_Page (Free_Map,
                         To_File_And_Page (Current_File, Free_Map_Index));
               New_Page := Get_Free_Page (Free_Map);
               exit Loop_Over_Files when New_Page /= 0;

               Free_Map_Index := Next_Free_Page (Free_Map);

               exit Loop_Over_Free_Maps when Free_Map_Index = 0;

            end loop Loop_Over_Free_Maps;

         end;

         Current_File := Current_File + 1;

         exit Loop_Over_Files when
           Current_File > Get_Last_File_Index (File.Info (1).Header);

      end loop Loop_Over_Files;

      if New_Page = 0 then
         Current_File := Get_Last_File_Index (File.Info (1).Header) + 1;
         Set_Last_File_Index (File.Info (1).Header, Current_File);
         Create_File (File, Current_File);
         Set_Page (Free_Map,
                   To_File_And_Page (Current_File, First_Free_Map_Index));
         New_Page := Get_Free_Page (Free_Map);
         pragma Assert (New_Page /= 0);
      end if;

      declare
         New_Location : constant File_And_Page :=
           To_File_And_Page (Current_File, New_Page);
      begin
         Files.Clear (File.File, New_Location);
         Page_Handles.New_Page (Page, New_Location);
      end;

      Page_Handles.Exclusive_Lock (Page);

      Locks.Unlock (File.File_Lock);

   end Allocate;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Handle) is
      procedure Free is new Ada.Unchecked_Deallocation (File_Handle_Record,
                                                        File_Handle);
      Blocks, Pages, Hits, Misses : Natural;
      Cache : Marlowe.Caches.File_Cache := File.Cache;
      Disk_File : Marlowe.Files.File_Type := File.File;
   begin
      Marlowe.Caches.Get_Cache_Statistics (File.Cache, Blocks, Pages,
                                           Hits, Misses);
--        Marlowe.Trace.Put_Line ("Cache block allocations:" &
--                              Natural'Image (Blocks));
--        Marlowe.Trace.Put_Line ("Cache pages:" & Natural'Image (Pages));
--        Marlowe.Trace.Put_Line ("Cache hits:" & Natural'Image (Hits));
--        Marlowe.Trace.Put_Line ("Cache misses:" & Natural'Image (Misses));
      Free (File);
      Marlowe.Caches.Close (Cache);
      Files.Close (Disk_File);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File : in out File_Handle;
                     Name : in     String)
   is
   begin
      File := new File_Handle_Record;
      if Marlowe.Allocation.Debug_Allocation then
         Marlowe.Allocation.Allocate (File.all'Size, "file-handle");
      end if;

      File.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Create_File (File, 1);
   end Create;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File (File  : in     File_Handle;
                          Index : in File_Index)
   is
      use Marlowe.Pages.File_Header;
      use Marlowe.Pages.Free_Map;
      Header   : File_Header_Page := new File_Header_Page_Record;
      Free_Map : Free_Map_Page    := new Free_Map_Page_Record;
   begin

      Files.Create (File.File, Index, Get_File_Name (File, Index));

      Initialise (Header, Index, 1);
      Files.Write (File.File, To_Page (Header).all);
      Release (Header);

      Initialise (Free_Map, To_File_And_Page (Index, 2));
      Files.Write (File.File, To_Page (Free_Map).all);
      Release (Free_Map);

      if Index = 1 then
         File.Cache := Marlowe.Caches.Create_Cache (File.File, Cache_Size);

         declare
            use Marlowe.File_Header_Page_Handles;
            Header : File_Header_Page_Handle renames File.Info (1).Header;
         begin
            Set_Cache (Header, File.Cache);
            Set_Page (Header, To_File_And_Page (Index, 1));
         end;
      end if;

   end Create_File;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (File     : in File_Handle;
                         Location : in File_And_Page)
   is
      use Marlowe.Free_Map_Page_Handles;
      Free_Map       : Free_Map_Page_Handle;
      File_Loc       : constant File_Index := Get_File (Location);
      Page_Loc       : constant Page_Index := Get_Page (Location);
      Free_Map_Index : Page_Index          := First_Free_Map_Index;
   begin
      Set_Cache (Free_Map, File.Cache);
      loop
         Set_Page (Free_Map, To_File_And_Page (File_Loc, Free_Map_Index));
         Free_Map_Index := Next_Free_Page (Free_Map);
         exit when Free_Map_Index = 0 or Free_Map_Index > Page_Loc;
      end loop;

      Free (Free_Map, Page_Loc);

   end Deallocate;

   ---------------------
   -- First_User_Page --
   ---------------------

   function First_User_Page (File : in File_Handle) return File_And_Page is
      pragma Unreferenced (File);
   begin
      return To_File_And_Page (1, First_User_Page_Index);
   end First_User_Page;

   --------------------------
   -- Get_Cache_Statistics --
   --------------------------

   procedure Get_Cache_Statistics (File   : in     File_Handle;
                                   Blocks :    out Natural;
                                   Pages  :    out Natural;
                                   Hits   :    out Natural;
                                   Misses :    out Natural)
   is
   begin
      Caches.Get_Cache_Statistics (File.Cache, Blocks, Pages, Hits, Misses);
   end Get_Cache_Statistics;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (File  : in File_Handle;
                           Index : in File_Index)
                          return String
   is
      Base_Name : constant String :=
        Ada.Strings.Unbounded.To_String (File.Name);
   begin
      if Index = 1 then
         return Base_Name;
      else
         return Base_Name & Integer'Image (-Integer (Index));
      end if;
   end Get_File_Name;

   ----------
   -- Open --
   ----------

   procedure Open (File : in out File_Handle;
                   Name : in     String)
   is
   begin
      File := new File_Handle_Record;
      if Marlowe.Allocation.Debug_Allocation then
         Marlowe.Allocation.Allocate (File.all'Size, "file-handle");
      end if;

      File.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Files.Open (File.File, 1, Name);
      File.Cache := Marlowe.Caches.Create_Cache (File.File, Cache_Size);
      Marlowe.File_Header_Page_Handles.Set_Cache
        (File.Info (1).Header, File.Cache);
      Marlowe.File_Header_Page_Handles.Set_Page
        (File.Info (1).Header, To_File_And_Page (1, 1));

      for I in 2 ..
        File_Header_Page_Handles.Get_Last_File_Index (File.Info (1).Header)
      loop
         Files.Open (File.File, I, Get_File_Name (File, I));
         Marlowe.File_Header_Page_Handles.Set_Cache
           (File.Info (I).Header, File.Cache);
         Marlowe.File_Header_Page_Handles.Set_Page
           (File.Info (I).Header, To_File_And_Page (I, 1));
      end loop;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File     : in     File_Handle;
                   Page     : in out Marlowe.Page_Handles.Page_Handle'Class;
                   Location : in     File_And_Page)
   is
      use Marlowe.Page_Handles;
   begin
      Set_Cache (Page, File.Cache);
      Set_Page  (Page, Location);
   end Read;

end Marlowe.File_Handles;


