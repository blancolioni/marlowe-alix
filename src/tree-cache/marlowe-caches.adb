with Ada.Unchecked_Deallocation;

with Marlowe.Allocation;
with Marlowe.Debug;
with Marlowe.Debug_Classes;

package body Marlowe.Caches is

   Cache_Block_Size : constant := 4096;

   Cache_Hits, Cache_Misses : Natural := 0;

   subtype Cache_Block_Index is Page_Index range 0 .. Cache_Block_Size - 1;

   type Cache_Block_Record (Leaf : Boolean);
   type Cache_Block is access Cache_Block_Record;

   type Cache_Entry is
      record
         Cached_Page : Marlowe.Pages.Page;
         Info        : Cached_Page_Info;
      end record;

   Null_Cache_Entry : constant Cache_Entry := (null, null);

   type Array_Of_Sub_Blocks is array (Cache_Block_Index) of Cache_Block;
   type Array_Of_Elements   is array (Cache_Block_Index) of Cache_Entry;

   type Cache_Block_Record (Leaf : Boolean) is
      record
         Count : Natural     := 0;
         Next  : Cache_Block := null;
         case Leaf is
            when True =>
               Elements : Array_Of_Elements := (others => (null, null));
            when False =>
               Divider    : Page_Index := 0;
               Sub_Blocks : Array_Of_Sub_Blocks := (others => null);
         end case;
      end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Cache_Block_Record, Cache_Block);

   procedure Free is
      new Ada.Unchecked_Deallocation (Cached_Page_Info_Record,
                                      Cached_Page_Info);

   type Single_File_Cache is
      record
         Top       : Cache_Block;
         Max_Index : Page_Index   := 0;
         Depth     : Natural      := 0;
         Hits      : Natural      := 0;
         Misses    : Natural      := 0;
      end record;

   type Array_Of_Single_File_Caches is
     array (File_Index range 1 .. Files.Max_Files) of Single_File_Cache;

   type Free_Block_Array is array (Boolean) of Cache_Block;

   type File_Cache_Lock is access Marlowe.Locks.Lock;

   --  File_Cache_Record
   --  Stores details of the cache for a particular file
   --    File        : the Marlowe file being cached
   --    Cache       : cached pages
   --    Size        : current number of pages in cache
   --    Max_Size    : cache will not grow beyond this
   --    Free_Blocks : blocks which have been de-allocated
   --    LRU         : list of free cached pages, sorted by least recent use
   --    Used        : list of all pages in cache which are in use
   --    Allocations : number of allocated cache blocks
   --    Cache_Lock  : a lock for the entire cache

   type File_Cache_Record is
      record
         File        : Marlowe.Files.File_Type;
         Cache       : Array_Of_Single_File_Caches;
         Size        : Natural                     := 0;
         Max_Size    : Natural;
         Free_Blocks : Free_Block_Array;
         LRU         : List_Of_References.List;
         Used        : List_Of_References.List;
         Allocations : Natural := 0;
         Cache_Lock  : File_Cache_Lock;
      end record;

   package List_Of_Pages is
     new Ada.Containers.Doubly_Linked_Lists (Marlowe.Pages.Page,
                                             Marlowe.Pages."=");

   Free_Page_List : List_Of_Pages.List;

   package List_Of_Page_Infos is
      new Ada.Containers.Doubly_Linked_Lists (Cached_Page_Info);

   Free_Page_Info_List : List_Of_Page_Infos.List;

   function Get_Cache_Entry (Cache    : Single_File_Cache;
                             Index    : Page_Index)
                            return Cache_Entry;

   procedure Remove_Cache_Entry (Cache : File_Cache;
                                 Item  : File_And_Page);

   procedure Insert_Cache_Entry (Cache     : File_Cache;
                                 New_Entry : Cache_Entry);

   function Allocate_Block (Cache : File_Cache;
                            Leaf  : Boolean)
                           return Cache_Block;

   procedure Ensure_Size (Cache : File_Cache;
                          File  : File_Index;
                          Index : Page_Index);

   procedure Trace (Message : String);
   procedure Enter (Message : String);
   procedure Leave (Message : String);

   --------------------
   -- Allocate_Block --
   --------------------

   function Allocate_Block (Cache : File_Cache;
                            Leaf  : Boolean)
                           return Cache_Block
   is
      Result : Cache_Block;
   begin
      --  Enter ("Allocate_Block");

      if Cache.Free_Blocks (Leaf) = null then

         --  we have no free cache blocks, so allocate a new one
         Result := new Cache_Block_Record (Leaf);

         if Marlowe.Allocation.Debug_Allocation then
            Marlowe.Allocation.Allocate (Result.all'Size, "cache-block");
         end if;

         Cache.Allocations := Cache.Allocations + 1;

      else

         --  use the first free block
         Result := Cache.Free_Blocks (Leaf);
         Cache.Free_Blocks (Leaf) := Cache.Free_Blocks (Leaf).Next;
         Result.Next := null;

      end if;

      --  Leave ("Allocate_Block");
      return Result;
   end Allocate_Block;

   -----------
   -- Close --
   -----------

   procedure Close (Cache : in out File_Cache) is

      procedure Free_Blocks (Top : in out Cache_Block);
      --  Free all allocated blocks in the cache.
      --  This includes releasing the pages stored in them

      -----------------
      -- Free_Blocks --
      -----------------

      procedure Free_Blocks (Top : in out Cache_Block) is
      begin
         if Top /= null then

            if Top.Leaf then
               for I in Top.Elements'Range loop
                  Marlowe.Pages.Release (Top.Elements (I).Cached_Page);
                  Free (Top.Elements (I).Info);
               end loop;
            else
               for I in Top.Sub_Blocks'Range loop
                  Free_Blocks (Top.Sub_Blocks (I));
               end loop;
            end if;


            if Marlowe.Allocation.Debug_Allocation then
               Marlowe.Allocation.Deallocate (Top.all'Size, "cache-block");
            end if;

            Free (Top);
         end if;
      end Free_Blocks;

   begin
      --  Enter ("Close");

      declare
         use List_Of_References;
         It : Cursor;
      begin
         Flush (Cache);
         It := Cache.Used.First;
         while Has_Element (It) loop
            Remove_Cache_Entry (Cache, Element (It));
            Next (It);
         end loop;

         It := Cache.LRU.First;
         while Has_Element (It) loop
            Remove_Cache_Entry (Cache, Element (It));
            Next (It);
         end loop;
      end;

      for I in Cache.Free_Blocks'Range loop
         declare
            It : Cache_Block := Cache.Free_Blocks (I);
            Next : Cache_Block;
         begin
            while It /= null loop
               Next := It.Next;

               if Marlowe.Allocation.Debug_Allocation then
                  Marlowe.Allocation.Deallocate (It.all'Size, "cache-block");
               end if;

               Free (It);
               It := Next;
            end loop;
         end;
      end loop;

      for I in Cache.Cache'Range loop
         Free_Blocks (Cache.Cache (I).Top);
      end loop;

      declare
         procedure Free is
            new Ada.Unchecked_Deallocation (Marlowe.Locks.Lock,
                                            File_Cache_Lock);
      begin
         Free (Cache.Cache_Lock);
      end;

      declare
         procedure Free is
            new Ada.Unchecked_Deallocation (File_Cache_Record, File_Cache);
      begin
         if Marlowe.Allocation.Debug_Allocation then
            Marlowe.Allocation.Deallocate (Cache.all'Size, "file-cache");
         end if;

         Free (Cache);
      end;
      --  Leave ("Close");
   end Close;

   ------------------
   -- Create_Cache --
   ------------------

   function Create_Cache (File         : Marlowe.Files.File_Type;
                          Maximum_Size : Natural)
                         return File_Cache
   is
      Result : File_Cache;
   begin
      --  Enter ("Create_Cache");
      Result := new File_Cache_Record;

      if Marlowe.Allocation.Debug_Allocation then
         Marlowe.Allocation.Allocate (Result.all'Size, "file-cache");
      end if;

      Result.File     := File;
      Result.Max_Size := Maximum_Size;
      Result.Cache_Lock := new Marlowe.Locks.Lock;
      Result.LRU.Clear;
      Result.Used.Clear;

      --  Leave ("Create_Cache");
      return Result;
   end Create_Cache;

   -----------------
   -- Ensure_Size --
   -----------------

   procedure Ensure_Size (Cache : File_Cache;
                          File  : File_Index;
                          Index : Page_Index)
   is
      The_Cache : Single_File_Cache renames Cache.Cache (File);
   begin
      --  Enter ("Ensure_Size");
      while The_Cache.Max_Index < Index loop
         if The_Cache.Top = null then
            The_Cache.Top := Allocate_Block (Cache, True);
            The_Cache.Max_Index := Cache_Block_Size - 1;
            The_Cache.Depth := 1;
         else
            declare
               New_Cache : constant Cache_Block :=
                 Allocate_Block (Cache, False);
            begin
               New_Cache.Count := 1;
               New_Cache.Divider := The_Cache.Max_Index + 1;
               New_Cache.Sub_Blocks (0) := The_Cache.Top;
               The_Cache.Top := New_Cache;
            end;
            The_Cache.Max_Index :=
              Cache_Block_Size * (The_Cache.Max_Index + 1) - 1;
            The_Cache.Depth := The_Cache.Depth + 1;
         end if;
      end loop;
      --  Leave ("Ensure_Size");
   end Ensure_Size;

   -----------
   -- Enter --
   -----------

   procedure Enter (Message : String) is
   begin
      Trace ("ENTER: " & Message);
   end Enter;

   --------------------
   -- Exclusive_Lock --
   --------------------

   procedure Exclusive_Lock (Item : in Cached_Page_Info) is
   begin
      --  Enter ("Exclusive_Lock: " & Image (Item.Location));
      Marlowe.Locks.Exclusive_Lock (Item.Page_Lock);
      --  Leave ("Exclusive_Lock: " & Image (Item.Location));
   end Exclusive_Lock;

   ----------------------
   -- Exclusive_Locked --
   ----------------------

   function Exclusive_Locked (Item : in Cached_Page_Info) return Boolean is
   begin
      return Marlowe.Locks.Exclusive_Locked (Item.Page_Lock);
   end Exclusive_Locked;

   -----------
   -- Flush --
   -----------

   procedure Flush (Cache : in out File_Cache) is
      use List_Of_References;
      It : Cursor := Cache.Used.First;
   begin
      --  Enter ("Flush");
      while Has_Element (It) loop
         declare
            Ent : constant Cache_Entry :=
              Get_Cache_Entry (Cache.Cache (Get_File (Element (It))),
                               Get_Page (Element (It)));
         begin
            if Ent.Info.Dirty then
               Marlowe.Files.Write (Cache.File, Ent.Cached_Page.all);
               Ent.Info.Dirty := False;
            end if;
         end;
         Next (It);
      end loop;
      --  Leave ("Flush");
   end Flush;

   ---------------------
   -- Get_Cache_Entry --
   ---------------------

   function Get_Cache_Entry (Cache    : Single_File_Cache;
                             Index    : Page_Index)
                            return Cache_Entry
   is
      Block     : Cache_Block := Cache.Top;
      Sub_Index : Page_Index  := Index;
   begin
      --  Enter ("Get_Cache_Entry");

      if Index > Cache.Max_Index then
         Cache_Misses := Cache_Misses + 1;
         --  Leave ("Get_Cache_Entry");
         return Null_Cache_Entry;
      end if;

      while Block /= null and then not Block.Leaf loop
         declare
            Divider : constant Page_Index := Block.Divider;
         begin
            Block := Block.Sub_Blocks (Sub_Index / Divider);
            Sub_Index := Sub_Index mod Divider;
         end;
      end loop;

      if Block /= null then
         if Block.Elements (Sub_Index) /= Null_Cache_Entry then
            Cache_Hits := Cache_Hits + 1;
         else
            Cache_Misses := Cache_Misses + 1;
         end if;
         --  Leave ("Get_Cache_Entry");
         return Block.Elements (Sub_Index);
      else
--         WL.Trace.Put_Line ("Cache miss (null block):" & Index'Img);
         Cache_Misses := Cache_Misses + 1;
         --  Leave ("Get_Cache_Entry");
         return (null, null);
      end if;

   end Get_Cache_Entry;

   --------------------------
   -- Get_Cache_Statistics --
   --------------------------

   procedure Get_Cache_Statistics (Cache  : in     File_Cache;
                                   Blocks :    out Natural;
                                   Pages  :    out Natural;
                                   Hits   :    out Natural;
                                   Misses :    out Natural)
   is
   begin
      --  Enter ("Get_Cache_Statistics");
      Hits   := Cache_Hits;
      Misses := Cache_Misses;
      Pages  := Cache.Size;
      Blocks := Cache.Allocations;
      --        Cache_Hits        := 0;
      --        Cache_Misses      := 0;
      --        Cache.Allocations := 0;
      --  Leave ("Get_Cache_Statistics");
   end Get_Cache_Statistics;

   --------------
   -- Get_Page --
   --------------

   procedure Get_Page (From_Cache : in out File_Cache;
                       Location   : in     File_And_Page;
                       Result     :    out Marlowe.Pages.Page;
                       Info       :    out Cached_Page_Info)
   is
   begin
      --  Enter ("Get_Page: " & Image (Location));
      Trace ("Taking shared lock");
      Marlowe.Locks.Shared_Lock (From_Cache.Cache_Lock.all);

      declare
         File_Loc    : constant File_Index := Get_File (Location);
         Page_Loc    : constant Page_Index := Get_Page (Location);
         Found_Entry : Cache_Entry :=
           Get_Cache_Entry (From_Cache.Cache (File_Loc), Page_Loc);
      begin
         if Found_Entry = Null_Cache_Entry then

            Trace ("Removing shared lock");
            Marlowe.Locks.Shared_Unlock (From_Cache.Cache_Lock.all);

            Trace ("Taking exclusive lock");
            Marlowe.Locks.Exclusive_Lock (From_Cache.Cache_Lock.all);

            --  We ask again in case it was fetched between dropping the
            --  the shared lock and taking the exclusive one

            Found_Entry :=
              Get_Cache_Entry (From_Cache.Cache (File_Loc), Page_Loc);

         end if;

         if Found_Entry = Null_Cache_Entry then

            --  the cache size can grow beyond the maximum size
            --  FIXME: explain why

            while From_Cache.Size >= From_Cache.Max_Size loop
               exit when From_Cache.LRU.Is_Empty;
               declare
                  use List_Of_References;
                  It   : Cursor := From_Cache.LRU.First;
                  Item : constant File_And_Page := Element (It);
               begin
                  Remove_Cache_Entry (From_Cache, Item);
                  From_Cache.LRU.Delete (It);
                  From_Cache.Size := From_Cache.Size - 1;
               end;
            end loop;

            --  re-use a freed page if possible, otherwise allocate a
            --  new one

            if Free_Page_List.Is_Empty then
               Result := new Marlowe.Pages.Page_Record;
               if Marlowe.Allocation.Debug_Allocation then
                  Marlowe.Allocation.Allocate (Result.all'Size, "page");
               end if;

            else

               declare
                  It : List_Of_Pages.Cursor :=
                         Free_Page_List.First;
               begin
                  Result := List_Of_Pages.Element (It);
                  Free_Page_List.Delete (It);
               end;
            end if;

            --  Ask Marlowe to read the page from disk
            Marlowe.Files.Read (From_Cache.File, Location, Result.all);

            if Free_Page_Info_List.Is_Empty then
               Info   := new Cached_Page_Info_Record;
               if Marlowe.Allocation.Debug_Allocation then
                  Marlowe.Allocation.Allocate (Info.all'Size, "page-info");
               end if;
            else
               declare
                  It : List_Of_Page_Infos.Cursor :=
                         Free_Page_Info_List.First;
               begin
                  Info := List_Of_Page_Infos.Element (It);
                  Free_Page_Info_List.Delete (It);
               end;
            end if;

            Info.Last_Access := Ada.Calendar.Clock;
            Info.From_Cache  := From_Cache;
            Info.Location    := Location;
            Info.References  := 1;
            Info.Cached      := True;
            Info.Dirty       := False;
            From_Cache.Used.Append (Location);
            Info.Used_Cursor := From_Cache.Used.Last;
            Info.LRU_Cursor  := List_Of_References.No_Element;

            --  Note that Info.LRU is a cursor which points to this page's
            --  location in the global LRU list.  We use it to move the page
            --  to the back when it is accessed

            Found_Entry := (Result, Info);
            Insert_Cache_Entry (From_Cache, Found_Entry);

         else

            Result := Found_Entry.Cached_Page;
            Info   := Found_Entry.Info;
            Reference (Info);

         end if;
      end;

      if Marlowe.Locks.Exclusive_Locked (From_Cache.Cache_Lock.all) then
         Trace ("Removing exclusive lock");
         Marlowe.Locks.Unlock (From_Cache.Cache_Lock.all);
      else
         Trace ("Removing shared lock");
         Marlowe.Locks.Shared_Unlock (From_Cache.Cache_Lock.all);
      end if;

      --  Leave ("Get_Page: " & Image (Location));
   end Get_Page;

   ------------------------
   -- Insert_Cache_Entry --
   ------------------------

   procedure Insert_Cache_Entry (Cache     : File_Cache;
                                 New_Entry : Cache_Entry)
   is
      Block     : Cache_Block;
      Location  : constant File_And_Page := New_Entry.Info.Location;
      Sub_Cache  : Single_File_Cache renames
        Cache.Cache (Get_File (Location));
      Sub_Index : Page_Index    := Get_Page (Location);
   begin
      --  Enter ("Insert_Cache_Entry");
      Ensure_Size (Cache, Get_File (Location), Sub_Index);
      Block := Sub_Cache.Top;
      while not Block.Leaf loop
         declare
            Divider : constant Page_Index := Block.Divider;
         begin
            if Block.Sub_Blocks (Sub_Index / Divider) = null then
               if Divider = Cache_Block_Size then
                  Block.Sub_Blocks (Sub_Index / Divider) :=
                    Allocate_Block (Cache, True);
               else
                  Block.Sub_Blocks (Sub_Index / Divider) :=
                    Allocate_Block (Cache, False);
                  Block.Sub_Blocks (Sub_Index / Divider).Divider :=
                    Divider / Cache_Block_Size;
               end if;
               Block.Count := Block.Count + 1;
            end if;
            Block := Block.Sub_Blocks (Sub_Index / Divider);
            Sub_Index := Sub_Index mod Divider;
         end;
      end loop;

      pragma Assert (Block.Elements (Sub_Index) = Null_Cache_Entry);
      Block.Elements (Sub_Index) := New_Entry;
      Block.Count := Block.Count + 1;
      Cache.Size  := Cache.Size + 1;
      --  Leave ("Insert_Cache_Entry");
   end Insert_Cache_Entry;

   -----------
   -- Leave --
   -----------

   procedure Leave (Message : String) is
   begin
      Trace ("LEAVE: " & Message);
   end Leave;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Info : Cached_Page_Info) is
   begin
      Marlowe.Locks.Exclusive_Lock (Info.Info_Lock);
      --  Enter ("Reference: " & Image (Info.Location));
      if Info.References = 0 then
         if List_Of_References.Has_Element (Info.LRU_Cursor) then
            Info.From_Cache.LRU.Delete (Info.LRU_Cursor);
         else
            Info.From_Cache.Used.Delete (Info.Used_Cursor);
         end if;
         Info.From_Cache.Used.Append (Info.Location);
         Info.Used_Cursor := Info.From_Cache.Used.Last;
         Info.LRU_Cursor := List_Of_References.No_Element;
      end if;
      Info.References := Info.References + 1;
      --  Leave ("Reference: " & Image (Info.Location));
      Marlowe.Locks.Unlock (Info.Info_Lock);
   end Reference;

   ------------------------
   -- Remove_Cache_Entry --
   ------------------------

   procedure Remove_Cache_Entry (Cache : File_Cache;
                                 Item  : File_And_Page)
   is
      Sub_Cache  : Single_File_Cache renames Cache.Cache (Get_File (Item));
      Block      : Cache_Block := Cache.Cache (Get_File (Item)).Top;
      Sub_Index  : Page_Index  := Get_Page (Item);
      Path       : array (1 .. Sub_Cache.Depth) of Cache_Block;
      Index_Path : array (1 .. Sub_Cache.Depth) of Page_Index;
      Path_Count : Natural := 0;
   begin
      --  Enter ("Remove_Cache_Entry");
      while not Block.Leaf loop
         Path_Count := Path_Count + 1;
         Path (Path_Count) := Block;
         declare
            Divider : constant Page_Index := Block.Divider;
         begin
            Index_Path (Path_Count) := Sub_Index / Divider;
            Block := Block.Sub_Blocks (Sub_Index / Divider);
            Sub_Index := Sub_Index mod Divider;
         end;
      end loop;

      declare
         Old : Cache_Entry renames Block.Elements (Sub_Index);
      begin
         List_Of_Pages.Append (Free_Page_List, Old.Cached_Page);
         --  Marlowe.Allocation.Deallocate (Old.Info.all'Size, "page-info");
         --  Marlowe.Allocation.Deallocate (Old.all'Size, "cache-entry");
         List_Of_Page_Infos.Append (Free_Page_Info_List, Old.Info);
         Old := Null_Cache_Entry;
         --  Free (Old.Info);
         --  Free (Old);
      end;

      Block.Count := Block.Count - 1;

      while Path_Count > 0 and Block.Count = 0 loop
         Block.Next := Cache.Free_Blocks (Block.Leaf);
         Cache.Free_Blocks (Block.Leaf) := Block;
         Block := Path (Path_Count);
         Block.Sub_Blocks (Index_Path (Path_Count)) := null;
         Block.Count := Block.Count - 1;
         Path_Count := Path_Count - 1;
      end loop;
      --  Leave ("Remove_Cache_Entry");
   end Remove_Cache_Entry;

   ------------------
   -- Set_Accessed --
   ------------------

   procedure Set_Accessed (Info : Cached_Page_Info) is
   begin
      --  Enter ("Set_Accessed");
      Info.Last_Access := Ada.Calendar.Clock;
      --  Leave ("Set_Accessed");
   end Set_Accessed;

   ---------------
   -- Set_Dirty --
   ---------------

   procedure Set_Dirty (Info : Cached_Page_Info) is
   begin
      --  Enter ("Set_Dirty");
      Info.Dirty := True;
      --  Leave ("Set_Dirty");
   end Set_Dirty;

   -----------------
   -- Shared_Lock --
   -----------------

   procedure Shared_Lock (Item : in Cached_Page_Info) is
   begin
      --  Enter ("Shared_Lock");
      Marlowe.Locks.Shared_Lock (Item.Page_Lock);
      --  Leave ("Shared_Lock");
   end Shared_Lock;

   -------------------
   -- Shared_Locked --
   -------------------

   function Shared_Locked (Item : in Cached_Page_Info) return Boolean is
   begin
      return Marlowe.Locks.Shared_Locked (Item.Page_Lock);
   end Shared_Locked;

   -------------------
   -- Shared_Unlock --
   -------------------

   procedure Shared_Unlock (Item : in Cached_Page_Info) is
   begin
      --  Enter ("Shared_Unlock");
      Marlowe.Locks.Shared_Unlock (Item.Page_Lock);
      --  Leave ("Shared_Unlock");
   end Shared_Unlock;

   -----------
   -- Trace --
   -----------

   procedure Trace (Message : String) is
   begin
      null;
--        if Marlowe.Debug.Enabled (Marlowe.Debug_Classes.Cache) then
--           Marlowe.Trace.Put_Line ("CACHE: " & Message);
--        end if;
   end Trace;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : in Cached_Page_Info) is
   begin
      --  Enter ("Unlock");
      Marlowe.Locks.Unlock (Item.Page_Lock);
      --  Leave ("Unlock");
   end Unlock;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Info : Cached_Page_Info) is

      Loc : constant File_And_Page := Info.Location;

   begin

      --  Enter ("Unreference: " & Image (Loc));

      Trace ("Current reference count:" &
             Info.References'Img);

      Trace ("Taking exlusive lock");
      Marlowe.Locks.Exclusive_Lock (Info.From_Cache.Cache_Lock.all);

      Info.References := Info.References - 1;

      if Info.References = 0 then
         if Info.Dirty then
            declare
               Ent : constant Cache_Entry :=
                 Get_Cache_Entry
                 (Info.From_Cache.Cache (Get_File (Info.Location)),
                  Get_Page (Info.Location));
            begin
               Marlowe.Files.Write (Info.From_Cache.File,
                                    Ent.Cached_Page.all);
               Info.Dirty := False;
            end;
         end if;

         Info.From_Cache.Used.Delete (Info.Used_Cursor);
         Info.Used_Cursor := List_Of_References.No_Element;
         Info.From_Cache.LRU.Append (Info.Location);
         Info.LRU_Cursor := Info.From_Cache.LRU.Last;
      end if;

      Trace ("New reference count:" &
             Info.References'Img);

      Trace ("Removing exlusive lock");
      Marlowe.Locks.Unlock (Info.From_Cache.Cache_Lock.all);
      --  Leave ("Unreference: " & Image (Loc));

   end Unreference;

   -----------------
   -- Update_Lock --
   -----------------

   procedure Update_Lock (Item : in Cached_Page_Info) is
   begin
      --  Enter ("Update_Lock");
      Marlowe.Locks.Update_Lock (Item.Page_Lock);
      --  Leave ("Update_Lock");
   end Update_Lock;

   -------------------
   -- Update_Locked --
   -------------------

   function Update_Locked (Item : in Cached_Page_Info) return Boolean is
   begin
      return Marlowe.Locks.Update_Locked (Item.Page_Lock);
   end Update_Locked;

   --------------------------
   -- Upgrade_To_Exclusive --
   --------------------------

   procedure Upgrade_To_Exclusive (Item : in Cached_Page_Info) is
   begin
      --  Enter ("Upgrade_To_Exclusive");
      Marlowe.Locks.Upgrade_To_Exclusive (Item.Page_Lock);
      --  Leave ("Upgrade_To_Exclusive");
   end Upgrade_To_Exclusive;

end Marlowe.Caches;
