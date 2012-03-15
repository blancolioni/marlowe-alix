with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Marlowe.Allocation;
--  with Marlowe.Debug;
--  with Marlowe.Debug_Classes;
with Marlowe.Locks;

with Marlowe.Trace;

package body Marlowe.Caches is

   Show_References : constant Boolean := False;
   pragma Unreferenced (Show_References);

   Cache_Hits, Cache_Misses : Natural := 0;

   package List_Of_Cached_Pages is
     new Ada.Containers.Doubly_Linked_Lists (Cached_Page_Info);

   type Cached_Page_Info_Record is limited
      record
         Cached_Page : aliased Marlowe.Pages.Page_Record;
         Dirty       : Boolean            := False;
         Location    : File_And_Page;
         Last_Access : Ada.Calendar.Time;
         References  : Natural            := 1;
         From_Cache  : File_Cache;
         Page_Lock   : Marlowe.Locks.Lock;
         Info_Lock   : Marlowe.Locks.Lock;
         Position    : List_Of_Cached_Pages.Cursor;
         Next        : Cached_Page_Info;
      end record;

   function File_And_Page_Hash (Key : File_And_Page)
                                return Ada.Containers.Hash_Type;

   package Map_Of_Cached_Pages is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => File_And_Page,
        Element_Type    => Cached_Page_Info,
        Hash            => File_And_Page_Hash,
        Equivalent_Keys => "=");

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
         Size        : Natural                     := 0;
         Max_Size    : Natural;
         Allocations : Natural := 0;
         Cache_Lock  : File_Cache_Lock;
         LRU_Lock    : Marlowe.Locks.Lock;
         LRU_List    : List_Of_Cached_Pages.List;
         Hash_Table  : Map_Of_Cached_Pages.Map;
      end record;

   --  procedure Move_To_End (Info : Cached_Page_Info);

   function Get_Cached_Page
     (Cache    : File_Cache;
      Location : File_And_Page)
      return Cached_Page_Info;

   procedure Trace (Message : String);
   procedure Enter (Message : String);
   pragma Unreferenced (Enter);

   procedure Leave (Message : String);
   pragma Unreferenced (Leave);

   -----------
   -- Close --
   -----------

   procedure Close (Cache : in out File_Cache) is
      use List_Of_Cached_Pages;
      It : Cursor := Cache.LRU_List.First;
      procedure Free is
        new Ada.Unchecked_Deallocation (Cached_Page_Info_Record,
                                        Cached_Page_Info);

   begin
      while Has_Element (It) loop
         declare
            E : Cached_Page_Info := Element (It);
         begin
            Cache.LRU_List.Replace_Element (It, null);
            Next (It);
            Free (E);
         end;
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
      Ada.Text_IO.Put_Line ("Cache hits:   " & Cache_Hits'Img);
      Ada.Text_IO.Put_Line ("Cache misses: " & Cache_Misses'Img);

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

      --  Leave ("Create_Cache");
      return Result;
   end Create_Cache;

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

   ------------------------
   -- File_And_Page_Hash --
   ------------------------

   function File_And_Page_Hash (Key : File_And_Page)
                                return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (Get_Page (Key));
   end File_And_Page_Hash;

   -----------
   -- Flush --
   -----------

   procedure Flush (Cache : in out File_Cache) is
      use List_Of_Cached_Pages;
      It : Cursor := Cache.LRU_List.First;
   begin
      while Has_Element (It) loop
         declare
            E : constant Cached_Page_Info := Element (It);
         begin
            if E.Dirty then
               Marlowe.Files.Write (Cache.File,
                                    E.Cached_Page);
            end if;

            Next (It);
         end;
      end loop;
   end Flush;

   ---------------------
   -- Get_Cache_Entry --
   ---------------------

--     function Get_Cache_Entry (Cache    : Single_File_Cache;
--                               Index    : Page_Index)
--                              return Cache_Entry
--     is
--        Block     : Cache_Block := Cache.Top;
--        Sub_Index : Page_Index  := Index;
--     begin
--        --  Enter ("Get_Cache_Entry");
--
--        if Index > Cache.Max_Index then
--           Cache_Misses := Cache_Misses + 1;
--           --  Leave ("Get_Cache_Entry");
--           return Null_Cache_Entry;
--        end if;
--
--        while Block /= null and then not Block.Leaf loop
--           declare
--              Divider : constant Page_Index := Block.Divider;
--           begin
--              Block := Block.Sub_Blocks (Sub_Index / Divider);
--              Sub_Index := Sub_Index mod Divider;
--           end;
--        end loop;
--
--        if Block /= null then
--           if Block.Elements (Sub_Index) /= Null_Cache_Entry then
--              Cache_Hits := Cache_Hits + 1;
--           else
--              Cache_Misses := Cache_Misses + 1;
--           end if;
--           --  Leave ("Get_Cache_Entry");
--           return Block.Elements (Sub_Index);
--        else
--  --         WL.Trace.Put_Line ("Cache miss (null block):" & Index'Img);
--           Cache_Misses := Cache_Misses + 1;
--           --  Leave ("Get_Cache_Entry");
--           return (null, null);
--        end if;
--
--     end Get_Cache_Entry;

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

   ---------------------
   -- Get_Cached_Page --
   ---------------------

   function Get_Cached_Page
     (Cache    : File_Cache;
      Location : File_And_Page)
      return Cached_Page_Info
   is
   begin
      if Cache.Hash_Table.Contains (Location) then
         declare
            use List_Of_Cached_Pages;
            E : Cached_Page_Info := Cache.Hash_Table.Element (Location);
         begin
            loop
               if E.Location = Location then
                  return E;
               elsif E.Next = null then
                  return null;
               else
                  E := E.Next;
               end if;
            end loop;
         end;
      else
         return null;
      end if;
   end Get_Cached_Page;

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
      if Marlowe.Trace.Tracing then
         Ada.Text_IO.Put_Line ("Taking shared lock: " & Image (Location));
      end if;

      Marlowe.Locks.Shared_Lock (From_Cache.Cache_Lock.all);

      Info := Get_Cached_Page (From_Cache, Location);

      if Info /= null then
         Result := Info.Cached_Page'Access;
         Cache_Hits := Cache_Hits + 1;
         Reference (Info);
      else

         Cache_Misses := Cache_Misses + 1;

         if Marlowe.Trace.Tracing then
            Ada.Text_IO.Put_Line ("Removing shared lock: " & Image (Location));
         end if;

         Marlowe.Locks.Shared_Unlock (From_Cache.Cache_Lock.all);

         if Marlowe.Trace.Tracing then
            Ada.Text_IO.Put_Line
              ("Taking exclusive lock: " & Image (Location));
         end if;

         Marlowe.Locks.Exclusive_Lock (From_Cache.Cache_Lock.all);

         --  We ask again in case it was fetched between dropping the
         --  the shared lock and taking the exclusive one

         Info := Get_Cached_Page (From_Cache, Location);

         if Info /= null then
            Result := Info.Cached_Page'Access;
            Reference (Info);
         else
            if From_Cache.Size >= From_Cache.Max_Size then
               Info := From_Cache.LRU_List.First_Element;
               --  if Show_References then
               --     Ada.Text_IO.Put_Line ("Removing from cache: " &
               --                           Image (Info.Location) &
               --                           " (reference count =" &
               --                           Info.References'Img);
               --  end if;

               declare
                  It : Map_Of_Cached_Pages.Cursor :=
                    From_Cache.Hash_Table.Find (Info.Location);
                  E : Cached_Page_Info :=
                    Map_Of_Cached_Pages.Element (It);
                  F : Cached_Page_Info;
               begin
                  if E = Info then
                     if E.Next /= null then
                        From_Cache.Hash_Table.Replace_Element (It, E.Next);
                     else
                        From_Cache.Hash_Table.Delete (It);
                     end if;
                  else
                     while E /= Info loop
                        F := E;
                        E := E.Next;
                     end loop;

                     F.Next := E.Next;
                  end if;
               end;

               From_Cache.LRU_List.Delete_First;
            else
               Info := new Cached_Page_Info_Record;
               From_Cache.Size := From_Cache.Size + 1;
               if From_Cache.Size = From_Cache.Max_Size then
                  Ada.Text_IO.Put_Line ("Maximum cache reached");
               elsif From_Cache.Size mod 4096 = 0 then
                  Ada.Text_IO.Put (Natural'Image (From_Cache.Size / 4096));
                  Ada.Text_IO.Flush;
               end if;
            end if;

            Result := Info.Cached_Page'Access;

            --  Ask Marlowe to read the page from disk
            Marlowe.Files.Read (From_Cache.File, Location, Info.Cached_Page);

            Info.Last_Access := Ada.Calendar.Clock;
            Info.From_Cache  := From_Cache;
            Info.Location    := Location;
            Info.References  := 1;
            Info.Dirty       := False;
            Info.Next        := null;

            --  From_Cache.LRU_List.Append (Info);
            Info.Position    := List_Of_Cached_Pages.No_Element;

            if From_Cache.Hash_Table.Contains (Location) then
               declare
                  E : Cached_Page_Info :=
                    From_Cache.Hash_Table.Element (Location);
               begin
                  while E.Next /= null loop
                     E := E.Next;
                  end loop;
                  E.Next := Info;
               end;
            else
               From_Cache.Hash_Table.Insert (Location, Info);
            end if;

         end if;
      end if;

      if Marlowe.Locks.Exclusive_Locked (From_Cache.Cache_Lock.all) then
         if Marlowe.Trace.Tracing then
            Ada.Text_IO.Put_Line
              ("Removing exclusive lock: " & Image (Location));
         end if;

         Marlowe.Locks.Unlock (From_Cache.Cache_Lock.all);
      else
         if Marlowe.Trace.Tracing then
            Ada.Text_IO.Put_Line ("Removing shared lock: " & Image (Location));
         end if;

         Marlowe.Locks.Shared_Unlock (From_Cache.Cache_Lock.all);
      end if;

      --  Leave ("Get_Page: " & Image (Location));
   end Get_Page;

   ------------------------
   -- Insert_Cache_Entry --
   ------------------------

--     procedure Insert_Cache_Entry (Cache     : File_Cache;
--                                   New_Entry : Cache_Entry)
--     is
--        Block     : Cache_Block;
--        Location  : constant File_And_Page := New_Entry.Info.Location;
--        Sub_Cache  : Single_File_Cache renames
--          Cache.Cache (Get_File (Location));
--        Sub_Index : Page_Index    := Get_Page (Location);
--     begin
--        --  Enter ("Insert_Cache_Entry");
--        Ensure_Size (Cache, Get_File (Location), Sub_Index);
--        Block := Sub_Cache.Top;
--        while not Block.Leaf loop
--           declare
--              Divider : constant Page_Index := Block.Divider;
--           begin
--              if Block.Sub_Blocks (Sub_Index / Divider) = null then
--                 if Divider = Cache_Block_Size then
--                    Block.Sub_Blocks (Sub_Index / Divider) :=
--                      Allocate_Block (Cache, True);
--                 else
--                    Block.Sub_Blocks (Sub_Index / Divider) :=
--                      Allocate_Block (Cache, False);
--                    Block.Sub_Blocks (Sub_Index / Divider).Divider :=
--                      Divider / Cache_Block_Size;
--                 end if;
--                 Block.Count := Block.Count + 1;
--              end if;
--              Block := Block.Sub_Blocks (Sub_Index / Divider);
--              Sub_Index := Sub_Index mod Divider;
--           end;
--        end loop;
--
--        pragma Assert (Block.Elements (Sub_Index) = Null_Cache_Entry);
--        Block.Elements (Sub_Index) := New_Entry;
--        Block.Count := Block.Count + 1;
--        Cache.Size  := Cache.Size + 1;
--        --  Leave ("Insert_Cache_Entry");
--     end Insert_Cache_Entry;

   -----------
   -- Leave --
   -----------

   procedure Leave (Message : String) is
   begin
      Trace ("LEAVE: " & Message);
   end Leave;

   -----------------
   -- Move_To_End --
   -----------------

--     procedure Move_To_End (Info : Cached_Page_Info) is
--     begin
--        Info.From_Cache.LRU_List.Delete (Info.Position);
--        Info.From_Cache.LRU_List.Append (Info);
--        Info.Position := Info.From_Cache.LRU_List.Last;
--     end Move_To_End;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (From_Cache : in out File_Cache;
                       Location   : in     File_And_Page;
                       Result     :    out Marlowe.Pages.Page;
                       Info       :    out Cached_Page_Info)
   is
   begin
      --  Enter ("Get_Page: " & Image (Location));
      if Marlowe.Trace.Tracing then
         Ada.Text_IO.Put_Line ("New_Page: taking exclusive lock: " &
                               Image (Location));
      end if;

      Marlowe.Locks.Exclusive_Lock (From_Cache.Cache_Lock.all);

      Info := Get_Cached_Page (From_Cache, Location);
      if Info /= null then
         Info.From_Cache.LRU_List.Delete (Info.Position);
      else
         Info := new Cached_Page_Info_Record;
         From_Cache.Size := From_Cache.Size + 1;
         if From_Cache.Size >= From_Cache.Max_Size then
            Ada.Text_IO.Put_Line ("Maximum cache reached");
         elsif From_Cache.Size mod 4096 = 0 then
            Ada.Text_IO.Put (Natural'Image (From_Cache.Size / 4096));
            Ada.Text_IO.Flush;
         end if;

         if From_Cache.Hash_Table.Contains (Location) then
            declare
               E : Cached_Page_Info :=
                     From_Cache.Hash_Table.Element (Location);
            begin
               while E.Next /= null loop
                  E := E.Next;
               end loop;
               E.Next := Info;
            end;
         else
            From_Cache.Hash_Table.Insert (Location, Info);
         end if;
      end if;

      Result := Info.Cached_Page'Access;

      Info.Last_Access := Ada.Calendar.Clock;
      Info.From_Cache  := From_Cache;
      Info.Location    := Location;
      Info.References  := 1;
      Info.Dirty       := True;
      Info.Next        := null;
      Info.Position    := List_Of_Cached_Pages.No_Element;

      if Marlowe.Trace.Tracing then
         Ada.Text_IO.Put_Line ("New_Page: Removing exclusive lock: " &
                               Image (Location));
      end if;

      Marlowe.Locks.Unlock (From_Cache.Cache_Lock.all);

   end New_Page;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Info : Cached_Page_Info) is
   begin

      --  if Show_References then
      if Marlowe.Trace.Tracing then
         Ada.Text_IO.Put_Line ("Reference: " & Image (Info.Location) &
                               " refcount =" &
                               Info.References'Img);
      end if;

      --  end if;

      Marlowe.Locks.Exclusive_Lock (Info.Info_Lock);

      --  Enter ("Reference: " & Image (Info.Location));
      Info.References := Info.References + 1;

      if Info.References = 1 then

         Marlowe.Locks.Exclusive_Lock (Info.From_Cache.LRU_Lock);

         if not List_Of_Cached_Pages.Has_Element (Info.Position) then
            Ada.Text_IO.Put_Line ("unexpected empty element");
         end if;

         Info.From_Cache.LRU_List.Delete (Info.Position);
         Marlowe.Locks.Unlock (Info.From_Cache.LRU_Lock);

         Info.Position := List_Of_Cached_Pages.No_Element;
      end if;

      --  Leave ("Reference: " & Image (Info.Location));
      Marlowe.Locks.Unlock (Info.Info_Lock);
   end Reference;

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
      --  Enter ("Shared_Lock: " & Image (Item.Location) &
      --           Natural'Image (Marlowe.Locks.Shared_Lock_Count
      --                            (Item.Page_Lock)));
      Marlowe.Locks.Shared_Lock (Item.Page_Lock);
      --  Leave ("Shared_Lock: " & Image (Item.Location) &
      --           Natural'Image (Marlowe.Locks.Shared_Lock_Count
      --                            (Item.Page_Lock)));
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
      --  Enter ("Shared_Unlock: " & Image (Item.Location) &
      --           Natural'Image (Marlowe.Locks.Shared_Lock_Count
      --                            (Item.Page_Lock)));
      Marlowe.Locks.Shared_Unlock (Item.Page_Lock);
      --  Leave ("Shared_Unlock: " & Image (Item.Location) &
      --           Natural'Image (Marlowe.Locks.Shared_Lock_Count
      --                            (Item.Page_Lock)));
   end Shared_Unlock;

   -----------
   -- Trace --
   -----------

   procedure Trace (Message : String) is
   begin
      if False and then Marlowe.Trace.Tracing then
         Marlowe.Trace.Trace ("CACHE: " & Message);
      end if;
   end Trace;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : in Cached_Page_Info) is
   begin
      --  Enter ("Exclusive_Unlock: " & Image (Item.Location));
      Marlowe.Locks.Unlock (Item.Page_Lock);
      --  Leave ("Exclusive_Unlock: " & Image (Item.Location));
   end Unlock;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Info : Cached_Page_Info) is
   begin

--      if Show_References then
      if Marlowe.Trace.Tracing then
         Ada.Text_IO.Put_Line ("Unreference: " & Image (Info.Location) &
                               " refcount =" &
                               Info.References'Img);
         Ada.Text_IO.Flush;
      end if;

  --    end if;

      Marlowe.Locks.Exclusive_Lock (Info.Info_Lock);
      Marlowe.Locks.Shared_Lock (Info.From_Cache.Cache_Lock.all);

      if Info.Dirty then
         Marlowe.Files.Write (Info.From_Cache.File,
                              Info.Cached_Page);
         Info.Dirty := False;
      end if;

      Info.References := Info.References - 1;

      if Info.References = 0 then
         Marlowe.Locks.Exclusive_Lock (Info.From_Cache.LRU_Lock);
         Info.From_Cache.LRU_List.Append (Info);
         Info.Position := Info.From_Cache.LRU_List.Last;
         Marlowe.Locks.Unlock (Info.From_Cache.LRU_Lock);

         --  if Show_References then
         if Marlowe.Trace.Tracing then
            Ada.Text_IO.Put_Line ("Returning to pool: " &
                                  Image (Info.Location));
         end if;

         --  end if;
      end if;

      Marlowe.Locks.Shared_Unlock (Info.From_Cache.Cache_Lock.all);
      Marlowe.Locks.Unlock (Info.Info_Lock);
      if Marlowe.Trace.Tracing then
         Ada.Text_IO.Put_Line ("exit unreference " & Image (Info.Location));
      end if;

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
