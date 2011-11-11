with Marlowe.Trace;

with Marlowe.Exceptions;

package body Marlowe.Btree.Operations is

   function Image (M : Mark) return String;

   procedure Split_Child (Child  : in out Pages.Btree_Page;
                          Index  : in     Pages.Key_Index;
                          Parent : in out Pages.Btree_Page);

   procedure Insert_Nonfull (Page    : in out Pages.Btree_Page;
                             Key     : in     Key_Type;
                             Element : in     Marlowe.Database_Index);


   function First_Scan (Page      : Pages.Btree_Page;
                        Slot      : Pages.Key_Index;
                        Scan      : Scan_Type;
                        Interval  : Interval_Type)
                       return Mark;

   function Match (Current : Mark)
                  return Boolean;

   ----------------
   -- Check_Tree --
   ----------------

   procedure Check_Tree is
      use Pages;

      procedure Check_Page (P : Btree_Page) is
      begin
         if Is_Leaf (P) then
            null;
         else
            for I in Key_Index range 1 .. Number_Of_Keys (P) loop
               declare
                  Child : constant Btree_Page :=
                    Get_Page (Get_Child (P, I));
               begin
                  if Get_Parent (Child) /= P then
                     Marlowe.Trace.Put_Line ("Child" &
                                        Page_Index'Image (Get_Index (Child)) &
                                        " of page" &
                                        Page_Index'Image (Get_Index (P)) &
                                        " thinks that its parent is" &
                                        Page_Index'Image (Get_Parent (Child)));
                  end if;
                  Check_Page (Child);
               end;
            end loop;
         end if;
      end Check_Page;
   begin
      Check_Page (Get_Page (Disk_Pages.Root_Page));
   end Check_Tree;

   ------------
   -- Delete --
   ------------

   procedure Delete (Key     : Key_Type;
                     Element : Marlowe.Database_Index)
   is


   ----------------
   -- Empty_Mark --
   ----------------

   function Empty_Mark return Mark is
   begin
      return (False, Pages.Null_Page, 1, Forward, Closed, 0);
   end Empty_Mark;

   ----------------
   -- First_Scan --
   ----------------

   function First_Scan (Page      : Pages.Btree_Page;
                        Slot      : Pages.Key_Index;
                        Scan      : Scan_Type;
                        Interval  : Interval_Type)
                       return Mark
   is
      use Pages;
      Result : Mark := (True, Page, Slot, Scan, Interval,
                        Get_Element (Page, Slot), Get_Key (Page, Slot));
   begin

      if not Match (Result) then
         Next (Result);
      end if;

      return Result;

   end First_Scan;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (Current : Mark) return Database_Index is
   begin
      return Current.Element;
   end Get_Element;

   ----------------------
   -- Get_With_Element --
   ----------------------

   function Get_With_Element (Key     : Key_Type;
                              Element : Marlowe.Database_Index)
                             return Mark
   is
      Result : Mark := Search (Key, Forward, Closed);
   begin
      while Valid (Result) and then Get_Element (Result) /= Element loop
         Next (Result);
      end loop;

      pragma Assert (Valid (Result));
      pragma Assert (Get_Element (Result) = Element);

      return Result;
   end Get_With_Element;

   -----------
   -- Image --
   -----------

   function Image (M : Mark) return String is
   begin
      return '(' & M.Use_Key'Img & Page_Index'Image (Pages.Get_Index (M.Page)) &
        M.Slot'Img & M.Element'Img & ')';
   end Image;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Key     : in     Key_Type;
      Element : in     Marlowe.Database_Index)
   is
      use Pages;
      Root : Btree_Page;
   begin

      if Unique then
         if Search (Key) /= 0 then
            raise Marlowe.Exceptions.Duplicate_Key;
         end if;
      end if;

      Root := Get_Page (Disk_Pages.Root_Page);

      if Number_Of_Keys (Root) = Max_Keys_In_Page then
         declare
            New_Root  : Btree_Page := New_Page;
         begin
            Disk_Pages.Set_Root (Get_Index (New_Root));
            Set_Root (Root, False);
            Set_Leaf (New_Root, False);
            Set_Sentinel (New_Root, Get_Index (Root));
            Set_Parent (Root, Get_Index (New_Root));

            Split_Child (Root, 1, New_Root);
            Insert_Nonfull (New_Root, Key, Element);
         end;
      else
         Insert_Nonfull (Root, Key, Element);
      end if;

   end Insert;

   --------------------
   -- Insert_Nonfull --
   --------------------

   procedure Insert_Nonfull (Page    : in out Pages.Btree_Page;
                             Key     : in     Key_Type;
                             Element : in     Marlowe.Database_Index)
   is
      use Pages;
   begin
      if Is_Leaf (Page) then
         Insert (Page, Key, Element, 0);
      else
         declare
            Index : Key_Index  := Find_Key (Page, Key);
            Child : Btree_Page := Get_Child (Page, Index);
         begin
            if Number_Of_Keys (Child) = Max_Keys_In_Page then
               Split_Child (Child, Index, Page);
               if Get_Key (Page, Index) < Key then
                  Index := Index + 1;
                  Child := Get_Page (Get_Child (Page, Index));
               end if;
            end if;
            Insert_Nonfull (Child, Key, Element);
         end;
      end if;
   end Insert_Nonfull;

   -----------
   -- Match --
   -----------

   function Match (Current : Mark) return Boolean is
      use Pages;
   begin

      if Get_Index (Current.Page) = 0 then
         return False;
      elsif Current.Use_Key then
         declare
            use Pages;
            Page : Btree_Page renames Current.Page;
         begin
            if Get_Key (Page, Current.Slot) = Current.Key then
               return Current.Interval = Closed;
            else
               if Current.Scan = Forward then
                  return Current.Key < Get_Key (Page, Current.Slot);
               else
                  return Get_Key (Page, Current.Slot) < Current.Key;
               end if;
            end if;
         end;
      else
         return True;
      end if;
   end Match;

   ----------
   -- Next --
   ----------

   procedure Next (Current : in out Mark) is

      use Pages;

      function Next_OK return Boolean;

      -------------
      -- Next_OK --
      -------------

      function Next_OK return Boolean is
      begin
         return (Current.Scan = Forward and
                 Current.Slot < Number_Of_Keys (Current.Page))
           or (Current.Scan = Backward and Current.Slot > 1);
      end Next_OK;

      Current_Key : constant Key_Type :=
        Get_Key (Current.Page, Current.Slot);
      Success     : Boolean := False;

   begin

      if Is_Leaf (Current.Page) then

         if Next_OK then
            if Current.Scan = Forward then
               Current.Slot := Current.Slot + 1;
            else
               Current.Slot := Current.Slot - 1;
            end if;
            Success := True;
         else
            while Get_Parent (Current.Page) /= 0 loop
               declare
                  Parent : Btree_Page := Get_Parent (Current.Page);
                  Index  : Key_Index  := Find_Key (Parent, Current_Key);
               begin
                  if Current.Scan = Forward then
                     if not Is_Sentinel (Parent, Index) then
                        Current.Page := Parent;
                        Current.Slot := Index;
                        Success      := True;
                        exit;
                     else
                        Current.Page := Parent;
                     end if;
                  else
                     if Index > 1 then
                        Current.Page := Parent;
                        Current.Slot := Index - 1;
                        Success      := True;
                        exit;
                     else
                        Current.Page := Parent;
                     end if;
                  end if;
               end;
            end loop;
         end if;
      else
         if Current.Scan = Backward then
            declare
               Child : Btree_Page := Get_Child (Current.Page, Current.Slot);
            begin
               while Is_Sentinel (Child, Number_Of_Keys (Child)) loop
                  Child := Get_Child (Child, Number_Of_Keys (Child));
               end loop;
               Current.Page := Child;
               Current.Slot := Number_Of_Keys (Child);
            end;
            Success := True;
         elsif Next_OK then
            declare
               Child : Btree_Page :=
                 Get_Child (Current.Page, Current.Slot + 1);
            begin
               while not Is_Leaf (Child) loop
                  Child := Get_Child (Child, 1);
               end loop;
               Current.Page := Child;
               Current.Slot := 1;
            end;
            Success := True;
         else
            while Get_Parent (Current.Page) /= 0 loop
               declare
                  Parent : Btree_Page := Get_Parent (Current.Page);
                  Index  : Key_Index  := Find_Key (Parent, Current_Key);
               begin
                  if not Is_Sentinel (Parent, Index) then
                     Current.Page := Parent;
                     Current.Slot := Index;
                     Success      := True;
                     exit;
                  else
                     Current.Page := Parent;
                  end if;
               end;
            end loop;

         end if;

      end if;

      if Success then
         Current.Element := Get_Element (Current.Page, Current.Slot);
         if Current.Use_Key then
            Current.Key := Get_Key (Current.Page, Current.Slot);
         end if;
      else
         Current.Element := 0;
      end if;

   end Next;

   ------------
   -- Search --
   ------------

   function Search (Key     : Key_Type)
                   return Marlowe.Database_Index
   is
      use Pages;

      function Do_Search (Page : Btree_Page) return Marlowe.Database_Index is
         Index : Key_Index := Find_Key (Page, Key);
      begin

         if not Is_Sentinel (Page, Index) and then
           Get_Key (Page, Index) = Key
         then
            return Get_Element (Page, Index);
         elsif Is_Leaf (Page) then
            return 0;
         else
            return Do_Search (Get_Child (Page, Index));
         end if;
      end Do_Search;

   begin
      return Do_Search (Get_Page (Disk_Pages.Root_Page));
   end Search;

   ------------
   -- Search --
   ------------

   function Search (Key       : Key_Type;
                    Scan      : Scan_Type;
                    Interval  : Interval_Type)
                   return Mark
   is
      use Pages;

      function Do_Search (Page : Btree_Page) return Mark is
         Index : Key_Index := Find_Key (Page, Key, Scan = Forward);
      begin

         if not Is_Sentinel (Page, Index) and then
           Get_Key (Page, Index) = Key
         then
            if Interval = Closed then
               if Unique or else Is_Leaf (Page) then
                  return (True,
                          Page, Index, Scan, Interval,
                          Get_Element (Page, Index), Key);
               else
                  return Do_Search (Get_Child (Page, Index));
               end if;
            else
               declare
                  Result : Mark := (True, Page, Index, Scan, Interval,
                                    Get_Element (Page, Index), Key);
               begin
                  Next (Result);
                  if Unique or else
                    (Valid (Result) and then Result.Key /= Key)
                  then
                     return Result;
                  else
                     while Valid (Result) and then Result.Key = Key loop
                        Next (Result);
                     end loop;
                     return Result;
                  end if;
               end;
            end if;
         elsif Is_Leaf (Page) then
            return First_Scan (Page, Index, Scan, Interval);
         else
            return Do_Search (Get_Child (Page, Index));
         end if;
      end Do_Search;

      Root : Btree_Page;

   begin
      return Do_Search (Get_Root_Page);
   end Search;


   ------------
   -- Search --
   ------------

   function Search (Scan      : Scan_Type)
                   return Mark
   is
      use Pages;
      Page        : Btree_Page;
   begin

      Page := Get_Root_Page;

      while not Is_Leaf (Page) loop
         if Scan = Forward then
            Page := Get_Child (Page, 1);
         else
            Page := Get_Child (Page, Number_Of_Keys (Page));
         end if;
      end loop;

      if Scan = Forward then
         return (False, Page, 1, Scan, Open,
                 Get_Element (Page, 1));
      else
         return (False, Page, Number_Of_Keys (Page), Scan, Open,
                 Get_Element (Page, Number_Of_Keys (Page)));
      end if;

   end Search;


   -----------------
   -- Split_Child --
   -----------------

   procedure Split_Child (Child  : in out Pages.Btree_Page;
                          Index  : in     Pages.Key_Index;
                          Parent : in out Pages.Btree_Page)
   is
      use Pages;
      New_Child : Btree_Page;
   begin
      New_Child := New_Page;
      Split (Child, New_Child, Index, Parent);
   end Split_Child;

   -----------
   -- Valid --
   -----------

   function Valid (Current : Mark) return Boolean is
   begin
      return Current.Element /= 0;
   end Valid;

end Marlowe.Btree.Operations;

