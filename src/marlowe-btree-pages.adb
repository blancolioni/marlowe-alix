with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Marlowe.Btree.Cache;

package body Marlowe.Btree.Pages is

   protected type Mutex is
      entry Lock;
      entry Shared_Lock;
      procedure Unlock;
      procedure Shared_Unlock;
      function Shared_Lock_Count return Natural;
      function Exclusive_Lock return Boolean;

   private
      Locked : Boolean := False;
      Shared : Natural := 0;
   end Mutex;

   Cache_Mutex : Mutex;
   Lock_Mutex  : Mutex;

   procedure Free is new Ada.Unchecked_Deallocation (LRU_Element, LRU_List);

   type Cached_Page_Record is
      record
         Page    : Btree_Page_Access;
         Info    : Page_Info;
      end record;

   type Cached_Page is access Cached_Page_Record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Cached_Page_Record, Cached_Page);
   procedure Free is
      new Ada.Unchecked_Deallocation (Disk_Pages.Btree_Disk_Page,
                                      Btree_Page_Access);
   procedure Free is
      new Ada.Unchecked_Deallocation (Page_Info_Record, Page_Info);

   Max_Cache_Size : Natural := 8000;

   Cache_Size : Natural := 0;

   LRU : LRU_List;
   LRU_Last : LRU_List;

   function Get_Page_Index (From : Cached_Page) return Page_Index;

   package Cache is
      new Marlowe.Btree.Cache (Cached_Page, null, Get_Page_Index);

   procedure Insert_Cache_Entry (Page : Disk_Pages.Btree_Disk_Page);
   procedure Remove_Cache_Entry (Index : Page_Index);
   procedure Update_Access_Time (Index : Page_Index);
   procedure Wait_Clear (L     : in Page_Info;
                         Lock  : in Lock_Type;
                         Clear : in Lock_Status);

   procedure Flush (Cached : Cached_Page);

   -----------------
   -- Change_Lock --
   -----------------

   procedure Change_Lock (Page     : in out Btree_Page;
                          Success  :    out Boolean;
                          New_Lock : in     Lock_Type       := Shared;
                          Wait     : in     Boolean         := False)
   is
      pragma Unreferenced (Page);
      pragma Unreferenced (New_Lock);
      pragma Unreferenced (Wait);
   begin
      Success := False;
   end Change_Lock;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Flush;
      Cache.Close;
      Cache_Size := 0;
      while LRU /= null loop
         declare
            Tmp : constant LRU_List := LRU.Next;
         begin
            Free (LRU);
            LRU := Tmp;
         end;
      end loop;
      LRU := null;
      LRU_Last := null;
      Disk_Pages.Close;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (Path : String) is
   begin
      Disk_Pages.Create (Path);
   end Create;

   ---------------------
   -- Deallocate_Page --
   ---------------------

   procedure Deallocate_Page (Page : in out Btree_Page) is
   begin
      --  FIXME: implement this
      null;
   end Deallocate_Page;

   ----------------
   -- Delete_Key --
   ----------------

   procedure Delete_Key (From_Page : in Btree_Page;
                         Key       : in Key_Type)
   is
   begin
      Disk_Pages.Delete (From_Page.Page.all,
                         Find_Key (From_Page, Key));
      From_Page.Info.Dirty := True;
   end Delete_Key;

   ------------
   -- Delete --
   ------------

   procedure Delete (From_Page : in Btree_Page;
                     Index     : in Child_Index)
   is
   begin
      Disk_Pages.Delete (From_Page.Page.all, Index);
      From_Page.Info.Dirty := True;
   end Delete;

   ---------------
   -- Dump_Page --
   ---------------

   procedure Dump_Page (Page : in Btree_Page) is
   begin
      Disk_Pages.Dump_Page (Page.Page.all);
   end Dump_Page;

   --------------
   -- Find_Key --
   --------------

   function Find_Key
     (In_Page : Btree_Page;
      Key     : Key_Type;
      Forward : Boolean     := True)
      return Child_Index
   is
   begin
      return Disk_Pages.Find_Key (In_Page.Page.all, Key, Forward);
   end Find_Key;

   -----------
   -- Flush --
   -----------

   procedure Flush is
      It : constant LRU_List := LRU;
   begin
      Cache_Mutex.Lock;
      Cache.For_All_Cache_Entries (Flush'Access);

--        while It /= null loop
--           declare
--              Page : constant Cached_Page :=
--                Cache.Get_Cache_Entry (It.Element);
--           begin
--              if Page.Info.Dirty then
--                 Disk_Pages.Write_Page (Page.Page.all);
--              end if;
--           end;
--           declare
--              Tmp : LRU_List := It;
--           begin
--              It := It.Next;
--              Free (Tmp);
--           end;
--        end loop;
      Cache_Mutex.Unlock;
   end Flush;

   -----------
   -- Flush --
   -----------

   procedure Flush (Cached : Cached_Page) is
   begin
      if Cached.Info.Dirty then
         Disk_Pages.Write_Page (Cached.Page.all);
         Cached.Info.Dirty := False;
      end if;
   end Flush;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (From  : Btree_Page;
      Index : Child_Index)
      return Page_Index
   is
   begin
      return Disk_Pages.Get_Child (From.Page.all, Index);
   end Get_Child;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (From  : Btree_Page;
      Index : Child_Index)
      return Btree_Page
   is
   begin
      return Get_Page (Get_Child (From, Index));
   end Get_Child;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index  (Of_Page : Btree_Page) return Page_Index is
   begin
      return Disk_Pages.Get_Index (Of_Page.Page.all);
   end Get_Index;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (From_Page : Btree_Page;
      Index     : Key_Index)
      return Key_Type
   is
   begin
      return Disk_Pages.Get_Key (From_Page.Page.all, Index);
   end Get_Key;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (Index  : in     Page_Index) return Btree_Page is
      Page   : Btree_Page;
      Cached : Cached_Page;
   begin
      Cache_Mutex.Lock;

      Cached := Cache.Get_Cache_Entry (Index);

      if Cached = null then
         declare
            Disk_Page : Disk_Pages.Btree_Disk_Page;
         begin
            Disk_Pages.Read_Page (Index, Disk_Page);
            Insert_Cache_Entry (Disk_Page);
            Cached := Cache.Get_Cache_Entry (Index);
         end;
      end if;

      Page.Info := Cached.Info;
      Page.Page := Cached.Page;
      Page.Info.References := Page.Info.References + 1;

      Update_Access_Time (Index);

      Cache_Mutex.Unlock;

      return Page;

   end Get_Page;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (From   : in     Disk_Pages.Btree_Disk_Page)
                     return Btree_Page
   is
      Page   : Btree_Page;
      Index  : constant Page_Index := Disk_Pages.Get_Index (From);
      Cached : Cached_Page;
   begin
      Cache_Mutex.Lock;

      Cached := Cache.Get_Cache_Entry (Index);

      if Cached = null then
         Insert_Cache_Entry (From);
         Cached := Cache.Get_Cache_Entry (Index);
      end if;

      if not Cached.Info.Cached then
         Ada.Exceptions.Raise_Exception (Constraint_Error'Identity,
                                         "Page" & Index'Img &
                                         " should have been in cache");
      end if;

      if Cached.Info.LRU.Next /= null or Cached.Info.LRU.Prev /= null then
         --  This cached entry is scheduled to be evicted!
         --  Cancel that
         if Cached.Info.LRU = LRU then
            --  we were going to be next!
            LRU := Cached.Info.LRU.Next;
            LRU.Prev := null;
         else
            Cached.Info.LRU.Prev.Next := Cached.Info.LRU.Next;
         end if;

         if Cached.Info.LRU = LRU_Last then
            --  it was a while off
            LRU_Last := LRU_Last.Prev;
            LRU_Last.Next := null;
         else
            Cached.Info.LRU.Next.Prev := Cached.Info.LRU.Prev;
         end if;
         Cached.Info.LRU.Next := null;
         Cached.Info.LRU.Prev := null;
      end if;

      Page.Info := Cached.Info;
      Page.Page := Cached.Page;
      Page.Info.References := Page.Info.References + 1;

      Update_Access_Time (Index);

      Cache_Mutex.Unlock;

      return Page;

   end Get_Page;

   --------------------
   -- Get_Page_Index --
   --------------------

   function Get_Page_Index (From : Cached_Page) return Page_Index is
   begin
      return Disk_Pages.Get_Index (From.Page.all);
   end Get_Page_Index;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Of_Page : Btree_Page) return Page_Index is

   begin
      return Disk_Pages.Get_Parent (Of_Page.Page.all);
   end Get_Parent;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Of_Page : Btree_Page) return Btree_Page is
   begin
      return Get_Page (Get_Parent (Of_Page));
   end Get_Parent;

   -------------------
   -- Get_Root_Page --
   -------------------

   function Get_Root_Page return Btree_Page is
   begin
      return Get_Page (Disk_Pages.Root_Page);
   end Get_Root_Page;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Page   : in out Btree_Page;
      Index  : in     Page_Index;
      Leaf   : in     Boolean)
   is
   begin
      Disk_Pages.Initialise (Page.Page.all, Index, Leaf);
      Page.Info.Dirty := True;
   end Initialise;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (To_Page : in Btree_Page;
      Key     : in Key_Type;
      Child   : in Page_Index)
   is
   begin
      Disk_Pages.Insert (To_Page.Page.all, Key, Child);
      To_Page.Info.Dirty := True;
   end Insert;

   ------------------------
   -- Insert_Cache_Entry --
   ------------------------

   procedure Insert_Cache_Entry (Page : Disk_Pages.Btree_Disk_Page) is
      New_Entry : Cached_Page := new Cached_Page_Record;
   begin
      New_Entry.Page := new Disk_Pages.Btree_Disk_Page'(Page);
      New_Entry.Info := new Page_Info_Record;
      New_Entry.Info.Last_Access        := Ada.Calendar.Clock;
      New_Entry.Info.Cached := True;

      if Cache_Size >= Max_Cache_Size and LRU /= null then
         declare
            First : LRU_List   := LRU;
            Index : constant Page_Index := First.Element;
         begin
            Remove_Cache_Entry (Index);
            LRU := First.Next;
            Free (First);
            if LRU = null then
               LRU_Last := null;
            else
               LRU.Prev := null;
            end if;
         end;
         Cache_Size := Cache_Size - 1;
      end if;

      Cache_Size := Cache_Size + 1;

      New_Entry.Info.LRU := new LRU_Element'(null, null,
                                             Disk_Pages.Get_Index (Page));
      Cache.Insert_Cache_Entry (New_Entry);

   end Insert_Cache_Entry;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Page : Btree_Page) return Boolean is
   begin
      return Disk_Pages.Is_Leaf (Page.Page.all);
   end Is_Leaf;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Page : Btree_Page) return Boolean is
   begin
      return Disk_Pages.Is_Root (Page.Page.all);
   end Is_Root;

   ----------
   -- Lock --
   ----------

   procedure Lock (Page    : in out Btree_Page;
                   Success :    out Boolean;
                   Lock    : in     Lock_Type       := Shared;
                   Wait    : in     Boolean         := True)
   is
      Wait_Status : Lock_Status := (others => False);
      L           : Page_Info renames Page.Info;
   begin
      Lock_Mutex.Lock;
      L.Waiting (Lock) := L.Waiting (Lock) + 1;
      loop
         case Lock is
            when Clear =>
               Lock_Mutex.Unlock;
               raise Locking_Error;

            when Shared =>
               if not L.X_Locked and
                 L.Waiting (Exclusive) + L.Waiting (Update) = 0
               then
                  L.S_Locks := L.S_Locks + 1;
                  L.Locked (Shared) := True;
                  Success := True;
               else
                  Wait_Status (Exclusive) := True;
               end if;
            when Update =>
               if not L.X_Locked and L.U_Locks = 0 and
                 L.Waiting (Exclusive) = 0
               then
                  L.U_Locks := L.U_Locks + 1;
                  L.Locked (Update) := True;
                  Success := True;
               else
                  Wait_Status (Exclusive) := True;
                  Wait_Status (Update)    := True;
               end if;

            when Exclusive =>
               if not L.X_Locked and L.U_Locks = 0 and L.S_Locks = 0 and
                 (L.Waiting (Shared) + L.Waiting (Update) = 0 and
                  L.Waiting (Exclusive) = 1)
               then
                  L.X_Locked := True;
                  L.Locked (Exclusive) := True;
                  Success    := True;
               else
                  Wait_Status := (Clear => False, others => True);
               end if;
         end case;

         if Success or not Wait then
            L.Waiting (Lock) := L.Waiting (Lock) - 1;
         end if;

         Lock_Mutex.Unlock;

         exit when Success or not Wait;

         Wait_Clear (L, Lock, Wait_Status);
      end loop;

   end Lock;

   -----------
   -- Mutex --
   -----------

   protected body Mutex is

      entry Lock when not Locked and Shared = 0 is
      begin
         Locked := True;
      end Lock;

      entry Shared_Lock when not Locked is
      begin
         Shared := Shared + 1;
      end Shared_Lock;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

      procedure Shared_Unlock is
      begin
         Shared := Shared - 1;
      end Shared_Unlock;

      function Shared_Lock_Count return Natural is
      begin
         return Shared;
      end Shared_Lock_Count;

      function Exclusive_Lock return Boolean is
      begin
         return Locked;
      end Exclusive_Lock;

   end Mutex;

   --------------
   -- New_Page --
   --------------

   function New_Page return Btree_Page is
      Disk_Page : Disk_Pages.Btree_Disk_Page;
   begin
      Cache_Mutex.Lock;
      Disk_Pages.New_Page (Disk_Page);
      Cache_Mutex.Unlock;
      return Get_Page (Disk_Page);
   end New_Page;

   ---------------
   -- Null_Page --
   ---------------

   function Null_Page return Btree_Page is
   begin
      return (Ada.Finalization.Controlled with null, null);
      --  return (null, null);
   end Null_Page;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys (In_Page : Btree_Page) return Key_Count is
   begin
      return Disk_Pages.Number_Of_Keys (In_Page.Page.all);
   end Number_Of_Keys;

   ----------
   -- Open --
   ----------

   procedure Open (Path : String) is
   begin
      Disk_Pages.Open (Path);
   end Open;

   ------------------------
   -- Remove_Cache_Entry --
   ------------------------

   procedure Remove_Cache_Entry (Index : Page_Index) is
      Cached : Cached_Page := Cache.Get_Cache_Entry (Index);
   begin
      if Cached.Info.Dirty then
         Disk_Pages.Write_Page (Cached.Page.all);
      end if;

      Cached.Info.Cached := False;

      declare
         Tmp : LRU_List := Cached.Info.LRU;
      begin
         if Tmp.Prev /= null then
            Tmp.Prev.Next := Tmp.Next;
         else
            LRU := Tmp.Next;
         end if;
         if Tmp.Next /= null then
            Tmp.Next.Prev := Tmp.Prev;
         else
            LRU_Last := Tmp.Prev;
         end if;
         Free (Tmp);
      end;

      Cache.Remove_Cache_Entry (Index);

      Free (Cached.Page);
      Free (Cached.Info);
      Free (Cached);

--      Cached := Cache.Get_Cache_Entry (2961);

   end Remove_Cache_Entry;

   --------------------
   -- Set_Cache_Size --
   --------------------

   procedure Set_Cache_Size (To : Natural) is
   begin
      Max_Cache_Size := To;
   end Set_Cache_Size;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (In_Page : in Btree_Page;
                        Index   : in Child_Index;
                        Child   : in Page_Index)
   is
   begin
      pragma Assert (Index <= Number_Of_Keys (In_Page) + 1);
      Disk_Pages.Set_Child (In_Page.Page.all, Index, Child);
      In_Page.Info.Dirty := True;
   end Set_Child;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (In_Page : in Btree_Page;
                      Index   : in Key_Index;
                      Key     : in Key_Type)
   is
   begin
      pragma Assert (Index <= Number_Of_Keys (In_Page));
      Disk_Pages.Set_Key (In_Page.Page.all, Index, Key);
      In_Page.Info.Dirty := True;
   end Set_Key;

   --------------
   -- Set_Leaf --
   --------------

   procedure Set_Leaf (Page : in Btree_Page;
                       Leaf : in Boolean)
   is
   begin
      Disk_Pages.Set_Leaf (Page.Page.all, Leaf);
      Page.Info.Dirty := True;
   end Set_Leaf;

   ------------------------
   -- Set_Number_Of_Keys --
   ------------------------

   procedure Set_Number_Of_Keys (In_Page : in Btree_Page;
                                 Num     : in Key_Count)
   is
   begin
      Disk_Pages.Set_Number_Of_Keys (In_Page.Page.all, Num);
      In_Page.Info.Dirty := True;
   end Set_Number_Of_Keys;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (Of_Page : in Btree_Page;
                         Parent  : in Page_Index)
   is
   begin
      Disk_Pages.Set_Parent (Of_Page.Page.all, Parent);
      Of_Page.Info.Dirty := True;
   end Set_Parent;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (Of_Page : in Btree_Page;
                         Parent  : in Btree_Page)
   is
   begin
      Set_Parent (Of_Page, Get_Index (Parent));
   end Set_Parent;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root (Page : in Btree_Page)
   is
   begin
      Disk_Pages.Set_Root (Get_Index (Page));
   end Set_Root;

   -----------
   -- Split --
   -----------

   procedure Split
     (Source   : in out Btree_Page;
      New_Sib  : in out Btree_Page;
      Index    : in     Key_Index;
      Parent   : in out Btree_Page)
   is
   begin
      Disk_Pages.Split (Source.Page.all, New_Sib.Page.all, Index,
                        Parent.Page.all);
      if not Is_Leaf (New_Sib) then
         for I in 1 .. Number_Of_Keys (New_Sib) + 1 loop
            declare
               Child : constant Btree_Page := Get_Child (New_Sib, I);
            begin
               Set_Parent (Child, New_Sib);
            end;
         end loop;
      end if;

      Source.Info.Dirty := True;
      New_Sib.Info.Dirty := True;
      Parent.Info.Dirty := True;

   end Split;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Page : in out Btree_Page;
                     Lock : in     Lock_Type)
   is
      Success : Boolean;
      L : Page_Info renames Page.Info;
   begin
      Lock_Mutex.Lock;

      if Lock = Exclusive then
         if L.X_Locked then
            L.X_Locked := False;
            L.Locked (Exclusive) := False;
            Success := True;
         else
            L.Locked (Exclusive) := False;
            Lock_Mutex.Unlock;
            raise Locking_Error;
         end if;
      elsif Lock = Update then
         if L.U_Locks > 0 then
            L.U_Locks := L.U_Locks - 1;
            if L.U_Locks = 0 then
               L.Locked (Update) := False;
            end if;
            Success := True;
         else
            L.Locked (Update) := False;
            Lock_Mutex.Unlock;
            raise Locking_Error;
         end if;
      elsif Lock = Shared then
         if L.S_Locks > 0 then
            L.S_Locks := L.S_Locks - 1;
            if L.S_Locks = 0 then
               L.Locked (Shared) := False;
            end if;
            Success := True;
         else
            L.Locked (Shared) := False;
            Lock_Mutex.Unlock;
            raise Locking_Error;
         end if;
      else
         Lock_Mutex.Unlock;
         raise Locking_Error;
      end if;

      Lock_Mutex.Unlock;

   end Unlock;

   ------------------------
   -- Update_Access_Time --
   ------------------------

   procedure Update_Access_Time (Index : Page_Index) is
      Page : Cached_Page := Cache.Get_Cache_Entry (Index);
   begin
      Page.Info.Last_Access := Ada.Calendar.Clock;
--        if Page.Info.LRU.Next /= null then
--           declare
--              Tmp : LRU_List := Page.Info.LRU;
--           begin
--              if Tmp.Prev /= null then
--                 Tmp.Prev.Next := Tmp.Next;
--              else
--                 LRU := LRU.Next;
--              end if;
--              if Tmp.Next /= null then
--                 Tmp.Next.Prev := Tmp.Prev;
--              else
--                 LRU_Last := LRU_Last.Prev;
--              end if;
--              LRU_Last.Next := Tmp;
--              Tmp.Next := null;
--              Tmp.Prev := LRU_Last;
--              LRU_Last := Tmp;
--           end;
--        end if;

   end Update_Access_Time;

   ----------------
   -- Wait_Clear --
   ----------------

   procedure Wait_Clear (L     : in Page_Info;
                         Lock  : in Lock_Type;
                         Clear : in Lock_Status)
   is
      Success : Boolean;
   begin
      loop
         Success := True;
         Lock_Mutex.Lock;
         for I in Clear'Range loop
            if Clear (I) and
              (L.Locked (I) or (I /= Lock and L.Waiting (I) > 0))
            then
               Success := False;
               exit;
            end if;
         end loop;
         Lock_Mutex.Unlock;
         exit when Success;
         delay 0.001;
      end loop;
   end Wait_Clear;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Page : in out Btree_Page) is
   begin
      Page.Info := null;
      Page.Page := null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Page : in out Btree_Page) is
   begin
      if Page.Info /= null then
         Page.Info.References := Page.Info.References - 1;
         if Page.Info.References = 0 then
            if LRU_Last = null then
               LRU_Last := Page.Info.LRU;
               LRU      := LRU_Last;
            else
               LRU_Last.Next := Page.Info.LRU;
               LRU_Last.Next.Prev := LRU_Last;
               LRU_Last := LRU_Last.Next;
            end if;
         end if;
         Page.Info := null;
         Page.Page := null;
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Page : in out Btree_Page) is
   begin
      if Page.Info /= null then
         Page.Info.References := Page.Info.References + 1;
      end if;
   end Adjust;

end Marlowe.Btree.Pages;

