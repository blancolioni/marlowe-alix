with WL.Trace;

with Marlowe.Exceptions;

package body Marlowe.Keys is

   --  function Image (M : Mark) return String;

   procedure Split_Child (Child  : in out Pages.Btree_Page;
                          Index  : in     Pages.Key_Index;
                          Parent : in out Pages.Btree_Page);

   procedure Insert_Nonfull (Page    : in out Pages.Btree_Page;
                             Key     : in     Key_Type);

   function Match (Current : Mark)
                  return Boolean;

   procedure Dump_Page (Page : Pages.Btree_Page);

   -------------
   -- Tree_OK --
   -------------

   function Tree_OK return Boolean is
      use Marlowe.Btree;
      use Pages;

      Ok : Boolean := True;

      procedure Check_Page (P : Btree_Page);
      --  Checks an individual page, and calls itself recursively
      --  on the page's children

      procedure Check_Page (P : Btree_Page) is
      begin
         if Is_Leaf (P) then
            null;
         else
            for I in Child_Index range 1 .. Number_Of_Keys (P) + 1 loop
               declare
                  use Marlowe.Btree;
                  Child : constant Btree_Page :=
                    Get_Page (Get_Child (P, I));
               begin
                  if Get_Parent (Child) /= P then
                     WL.Trace.Put_Line
                       ("Child" &
                        Image (Get_Index (Child)) &
                        " of page" &
                        Image (Get_Index (P)) &
                        " thinks that its parent is" &
                        Image (Get_Parent (Child)));
                     OK := False;
                  end if;
                  if Number_Of_Keys (Child) < Min_Keys_In_Page then
                     WL.Trace.Put_Line
                       ("Child" &
                        Image (Get_Index (Child)) &
                        " of page" &
                        Image (Get_Index (P)) &
                        " has only" & Slot'Image (Number_Of_Keys (Child)) &
                        " keys");
                     OK := False;
                  end if;

                  if I <= Number_Of_Keys (P) then
                     for J in Key_Index
                       range 1 .. Number_Of_Keys (Child)
                     loop
                        if Get_Key (P, I) < Get_Key (Child, J) then
                           WL.Trace.Put_Line
                             ("Child" &
                              Image (Get_Index (Child)) &
                              " of page" &
                              Image (Get_Index (P)) &
                              " contains a key greater than its parent slot");
                           OK := False;
                        elsif Get_Key (P, I) = Get_Key (Child, J) then
                           WL.Trace.Put_Line
                             ("Child" &
                              Image (Get_Index (Child)) &
                              " of page" &
                              Image (Get_Index (P)) &
                              " contains a key equal to its parent slot");
                           OK := False;
                        end if;
                     end loop;
                  end if;

                  Check_Page (Child);
               end;
            end loop;
         end if;
      end Check_Page;
   begin
      Check_Page (Get_Root_Page);
      return OK;
   end Tree_OK;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Pages.Close;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File_Path   : in String;
                     Cache_Size  : in Natural := 8192)
   is
   begin
      Pages.Create (File_Path);
      Pages.Set_Cache_Size (Cache_Size);
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Key     : Key_Type) is

      use Marlowe.Btree;
      use Pages;

      procedure Do_Delete (Page : Pages.Btree_Page;
                           K    : Key_Type);

      --  Delete Key, starting from Page.
      --  Key must be either in Page, or in one of Page's children

      procedure Delete_Internal (Page   : Pages.Btree_Page;
                                 Index  : Key_Index;
                                 K      : Key_Type);
      --  Delete a key from an internal page

      procedure Delete_From_Subtree (Page   : Pages.Btree_Page;
                                     Index  : Child_Index;
                                     K      : Key_Type);
      --  Delete a key from a subtree

      -------------------------
      -- Delete_From_Subtree --
      -------------------------

      procedure Delete_From_Subtree (Page   : Pages.Btree_Page;
                                     Index  : Child_Index;
                                     K      : Key_Type)
      is
         Child : Btree_Page := Get_Child (Page, Index);
      begin
--           WL.Trace.Put_Line ("Delete_From_Subtree: page" &
--                              Image (Get_Index (Page)) &
--                              "; index" & Index'Img &
--                              "; key " & Image (K));

         if Number_Of_Keys (Child) > Min_Keys_In_Page then
            --  WL.Trace.Put_Line ("Case 3");
            Do_Delete (Child, K);
         elsif Index > 1 and then
           Number_Of_Keys (Get_Child (Page, Index - 1)) > Min_Keys_In_Page
         then
            --  WL.Trace.Put_Line ("Case 3a (i)");
            declare
               Sib_Key        : constant Key_Type :=
                 Get_Key (Page, Index - 1);

               Sib_Page       : constant Btree_Page :=
                 Get_Child (Page, Index - 1);
               Last_Sib_Key   : constant Key_Type :=
                 Get_Key (Sib_Page, Number_Of_Keys (Sib_Page));
               Last_Sib_Child : constant Page_Index :=
                 Get_Child (Sib_Page, Number_Of_Keys (Sib_Page) + 1);
            begin
               Delete (Page, Index - 1);
               Insert (Child, Sib_Key, Last_Sib_Child);
               if Is_Leaf (Sib_Page) then
                  Insert (Page, Last_Sib_Key, Get_Index (Sib_Page));
                  Delete (Sib_Page, Number_Of_Keys (Sib_Page));
               else
                  Set_Parent (Get_Page (Last_Sib_Child), Child);
                  Insert (Page, Get_Key (Sib_Page,
                                         Number_Of_Keys (Sib_Page)),
                          Get_Index (Sib_Page));
                  Delete (Sib_Page, Number_Of_Keys (Sib_Page) + 1);
               end if;

               Do_Delete (Child, K);
            end;
         elsif Index <= Number_Of_Keys (Page) and then
           Number_Of_Keys (Get_Child (Page, Index + 1)) > Min_Keys_In_Page
         then
            --  WL.Trace.Put_Line ("Case 3a (ii)");
            declare
               Sib_Key         : constant Key_Type   :=
                 Get_Key (Page, Index);
               Sib_Page        : constant Btree_Page :=
                 Get_Child (Page, Index + 1);
               First_Sib_Key   : constant Key_Type   :=
                 Get_Key (Sib_Page, 1);
               First_Sib_Child : constant Page_Index :=
                 Get_Child (Sib_Page, 1);
            begin
               Delete (Page, Index);
               if Is_Leaf (Child) then
                  Insert (Child, Sib_Key, Null_Page_Index);
               else
                  Set_Number_Of_Keys (Child, Number_Of_Keys (Child) + 1);
                  Set_Key (Child, Number_Of_Keys (Child), Sib_Key);
                  Set_Child (Child, Number_Of_Keys (Child) + 1,
                             First_Sib_Child);
               end if;
               --  Insert (Child, Sib_Key, First_Sib_Child);
               Delete (Sib_Page, 1);
               Insert (Page, First_Sib_Key, Get_Index (Child));
               if not Is_Leaf (Sib_Page) then
                  Set_Parent (Get_Page (First_Sib_Child), Child);
               end if;

               Do_Delete (Child, K);
            end;
         elsif Index > 1 then
            --  WL.Trace.Put_Line ("Case 3b");
            declare
               Left_Key   : constant Key_Type   := Get_Key (Page, Index - 1);
               Left_Child : constant Btree_Page := Get_Child (Page, Index - 1);
               Left_Keys  : constant Key_Count  := Number_Of_Keys (Left_Child);
            begin

               Delete_Key (Page, Left_Key);

               if Is_Leaf (Left_Child) then
                  Insert (Child, Left_Key, Null_Page_Index);
               else
                  Set_Parent (Get_Child (Left_Child, Left_Keys + 1),
                              Child);
                  Insert (Child, Left_Key,
                          Get_Child (Left_Child, Left_Keys + 1));
               end if;

               for I in 1 .. Left_Keys loop
                  if not Is_Leaf (Left_Child) then
                     Set_Parent (Get_Child (Left_Child, I), Child);
                  end if;
                  Insert (Child, Get_Key (Left_Child, I),
                          Get_Child (Left_Child, I));
               end loop;

               Do_Delete (Child, K);

            end;
         else
            --  WL.Trace.Put_Line ("Case 3c");
            declare
               Left_Key   : constant Key_Type   := Get_Key (Page, Index);
               Left_Child : constant Btree_Page := Get_Child (Page, Index);
               Left_Keys  : constant Key_Count  := Number_Of_Keys (Left_Child);
            begin

               Child := Get_Child (Page, Index + 1);

               Delete (Page, Index);

               if Is_Leaf (Left_Child) then
                  Insert (Child, Left_Key, Null_Page_Index);
               else
                  Set_Parent (Get_Child (Left_Child, Left_Keys + 1),
                              Child);
                  Insert (Child, Left_Key,
                          Get_Child (Left_Child, Left_Keys + 1));
               end if;

               for I in 1 .. Number_Of_Keys (Left_Child) loop
                  if not Is_Leaf (Left_Child) then
                     Set_Parent (Get_Child (Left_Child, I), Child);
                  end if;
                  Insert (Child, Get_Key (Left_Child, I),
                          Get_Child (Left_Child, I));
               end loop;

               Do_Delete (Child, K);

            end;
         end if;
      end Delete_From_Subtree;

      ---------------------
      -- Delete_Internal --
      ---------------------

      procedure Delete_Internal (Page   : Pages.Btree_Page;
                                 Index  : Key_Index;
                                 K      : Key_Type)
      is
         Left  : Btree_Page := Get_Child (Page, Index);
         Right : constant Btree_Page := Get_Child (Page, Index + 1);
         Sib   : Key_Type;
      begin
         if Number_Of_Keys (Left) > Min_Keys_In_Page then
            --  WL.Trace.Put_Line ("Case 2a");
            declare
               P : Btree_Page := Left;
            begin
               while not Is_Leaf (P) loop
                  P := Get_Child (P, Number_Of_Keys (P) + 1);
               end loop;
               Sib := Get_Key (P, Number_Of_Keys (P));
            end;
            Do_Delete (Left, Sib);
            Set_Key (Page, Index, Sib);
         elsif Number_Of_Keys (Right) > Min_Keys_In_Page then
            --  WL.Trace.Put_Line ("Case 2b");
            declare
               P : Btree_Page := Right;
            begin
               while not Is_Leaf (P) loop
                  P := Get_Child (P, 1);
               end loop;
               Sib := Get_Key (P, 1);
            end;
            Do_Delete (Right, Sib);
            Set_Key (Page, Index, Sib);
         else
            --  WL.Trace.Put_Line ("Case 2c");
            if not Is_Leaf (Left) then
               Set_Parent (Get_Child (Left, Number_Of_Keys (Left) + 1),
                           Right);
               Insert (Right, Get_Key (Page, Index),
                       Get_Child (Left, Number_Of_Keys (Left) + 1));
            end if;

            Delete (Page, Index);

            for I in 1 .. Number_Of_Keys (Left) loop
               if not Is_Leaf (Left) then
                  Set_Parent (Get_Child (Left, I), Right);
               end if;
               Insert (Right, Get_Key (Left, I), Get_Child (Left, I));
            end loop;
            Deallocate_Page (Left);

            if not Is_Leaf (Right) then
               Do_Delete (Right, K);
            end if;
         end if;
      end Delete_Internal;

      ---------------
      -- Do_Delete --
      ---------------

      procedure Do_Delete (Page : Pages.Btree_Page;
                           K    : Key_Type)
      is
         use Marlowe.Btree;
         use Pages;
      begin
         if Is_Leaf (Page) then
            Delete_Key (Page, K);
         else
            declare
               Index : constant Child_Index := Find_Key (Page, K);
            begin
               if Index <= Number_Of_Keys (Page) and then
                 Get_Key (Page, Index) = K
               then
                  Delete_Internal (Page, Index, K);
               else
                  Delete_From_Subtree (Page, Index, K);
               end if;
            end;
         end if;
      end Do_Delete;

   begin
      Do_Delete (Get_Root_Page, Key);
   end Delete;

   ----------------
   -- Empty_Mark --
   ----------------

   function Empty_Mark return Mark is
   begin
      return (False, Pages.Null_Page, 1, Forward);
   end Empty_Mark;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Current : Mark) return Key_Type is
   begin
      return Pages.Get_Key (Current.Page, Current.Slot);
   end Get_Key;

   ------------
   -- Exists --
   ------------

   function Exists (Key     : Key_Type)
     return Boolean
   is
      Result : constant Mark := Search (Key, Key, Closed, Closed, Forward);
   begin
      return Valid (Result);
   end Exists;

   -----------
   -- Image --
   -----------

   function Image (M : Mark) return String is
   begin
      return '(' & M.Use_Key'Img &
        Marlowe.Btree.Image (Pages.Get_Index (M.Page)) &
        M.Slot'Img & ')';
   end Image;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Key     : in     Key_Type)
   is
      use type Marlowe.Btree.Slot;
      use Pages;
      Root : Btree_Page;
   begin

      if Exists (Key) then
         raise Marlowe.Exceptions.Duplicate_Key;
      end if;

      Root := Get_Root_Page;

      if Number_Of_Keys (Root) = Max_Keys_In_Page then
         declare
            New_Root  : Btree_Page := New_Page;
         begin
            Set_Root (New_Root);
            Set_Leaf (New_Root, False);
            Set_Child (New_Root, 1, Get_Index (Root));
            Set_Parent (Root, Get_Index (New_Root));

            Split_Child (Root, 1, New_Root);
            Insert_Nonfull (New_Root, Key);
         end;
      else
         Insert_Nonfull (Root, Key);
      end if;

   end Insert;

   --------------------
   -- Insert_Nonfull --
   --------------------

   procedure Insert_Nonfull (Page    : in out Pages.Btree_Page;
                             Key     : in     Key_Type)
   is
      use Pages;
   begin
      if Is_Leaf (Page) then
         Insert (Page, Key, Marlowe.Btree.Null_Page_Index);
      else
         declare
            use type Marlowe.Btree.Slot;
            Index : Child_Index := Find_Key (Page, Key);
            Child : Btree_Page  := Get_Child (Page, Index);
         begin
            if Number_Of_Keys (Child) = Max_Keys_In_Page then
               Split_Child (Child, Index, Page);
               if Index <= Number_Of_Keys (Page) and then
                 Get_Key (Page, Index) < Key
               then
                  Child := Get_Page (Get_Child (Page, Index + 1));
               else
                  Child := Get_Page (Get_Child (Page, Index));
               end if;
            end if;
            Insert_Nonfull (Child, Key);
         end;
      end if;
   end Insert_Nonfull;

   -----------
   -- Match --
   -----------

   function Match (Current : Mark) return Boolean is
      use Btree;
      use Pages;
   begin

      if not Valid (Current) then
         return False;
      elsif Current.Use_Key then
         declare
            use Pages;
            Page : Btree_Page renames Current.Page;
            Key  : constant Key_Type := Get_Key (Page, Current.Slot);
         begin
            return Current.Finish_Interval = Unbounded
              or else (Key = Current.Start_Key and
                       Current.Start_Interval = Closed)
              or else (Key = Current.Finish_Key and
                       Current.Finish_Interval = Closed)
              or else (Current.Start_Key < Key and
                       Key < Current.Finish_Key);
         end;
      else
         return True;
      end if;
   end Match;

   ----------
   -- Next --
   ----------

   procedure Next (Current : in out Mark) is

      use Marlowe.Btree;
      use Pages;

      function Next_OK return Boolean;

      -------------
      -- Next_OK --
      -------------

      function Next_OK return Boolean is
      begin
         return (Current.Scan = Forward and
                 Current.Slot < Number_Of_Keys (Current.Page))
           or (Current.Scan = Backward and Current.Slot > 1)
           or (not Is_Leaf (Current.Page) and Current.Scan = Forward and
               Current.Slot <= Number_Of_Keys (Current.Page));
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
            while Get_Parent (Current.Page) /= Null_Page_Index loop
               declare
                  use type Marlowe.Btree.Slot;
                  Parent : constant Btree_Page := Get_Parent (Current.Page);
                  Index  : constant Child_Index  :=
                    Find_Key (Parent, Current_Key);
               begin
                  if Current.Scan = Forward then
                     if Index <= Number_Of_Keys (Parent) then
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
               while not Is_Leaf (Child) loop
                  Child := Get_Child (Child, Number_Of_Keys (Child) + 1);
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
            while Get_Parent (Current.Page) /= Null_Page_Index loop
               declare
                  use type Marlowe.Btree.Slot;
                  Parent : constant Btree_Page := Get_Parent (Current.Page);
                  Index  : constant Child_Index  :=
                    Find_Key (Parent, Current_Key);
               begin
                  if Index <= Number_Of_Keys (Parent) then
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
         if not Match (Current) then
            Current.Page := Null_Page;
         end if;
      else
         Current.Page := Null_Page;
      end if;

   end Next;

   ----------
   -- Open --
   ----------

   procedure Open (File_Path  : in String;
                   Cache_Size : in Natural := 8192)
   is
   begin
      Pages.Open (File_Path);
      Pages.Set_Cache_Size (Cache_Size);
   end Open;

   ------------
   -- Search --
   ------------

   function Search (Start           : Key_Type;
                    Finish          : Key_Type;
                    Start_Interval  : Interval_Type;
                    Finish_Interval : Interval_Type;
                    Scan            : Scan_Type)
                   return Mark
   is
      use Pages;

      function First_Key return Key_Type;
      --  Return either Start or Finish, depending on the
      --  value of Scan

      function First_Interval return Interval_Type;
      --  Return either Start_Interval or Finish_Interval,
      --  depending on the value of Scan

      function Do_Search (Page : Btree_Page) return Mark;
      --  Perform the search on the given Page, and recurse to
      --  child pages as necessary

      --------------------
      -- First_Interval --
      --------------------

      function First_Interval return Interval_Type is
      begin
         if Scan = Forward then
            return Start_Interval;
         else
            return Finish_Interval;
         end if;
      end First_Interval;

      ---------------
      -- First_Key --
      ---------------

      function First_Key return Key_Type is
      begin
         if Scan = Forward then
            return Start;
         else
            return Finish;
         end if;
      end First_Key;

      Key : constant Key_Type := First_Key;
      Interval : constant Interval_Type := First_Interval;

      ---------------
      -- Do_Search --
      ---------------

      function Do_Search (Page : Btree_Page) return Mark is
         use type Marlowe.Btree.Slot;
         Index  : constant Child_Index :=
           Find_Key (Page, Key, Scan = Forward);
         Result : Mark;
      begin

         if Index <= Number_Of_Keys (Page) then
            if Get_Key (Page, Index) = Key then
               --  Exact match
               Result := (True, Page, Index, Scan,
                          Start, Finish,
                          Start_Interval, Finish_Interval);

               if Interval = Closed then
                  if Match (Result) then
                     return Result;
                  else
                     return Empty_Mark;
                  end if;
               else
                  Next (Result);
                  return Result;
               end if;
            elsif Is_Leaf (Page) then
               Result := (True, Page, Index, Scan,
                          Start, Finish,
                          Start_Interval, Finish_Interval);
               if Scan = Backward then
                  Next (Result);
               end if;

               if Match (Result) then
                  return Result;
               else
                  return Empty_Mark;
               end if;
            else
               return Do_Search (Get_Child (Page, Index));
            end if;
         else
            if Is_Leaf (Page) then
               if Scan = Forward then
                  declare
                     P     : Btree_Page := Page;
                     P_Index : Child_Index := Index;
                  begin
                     while not Is_Root (P) loop
                        P       := Get_Parent (P);
                        P_Index := Find_Key (P, Key, Scan /= Backward);
                        exit when P_Index <= Number_Of_Keys (P);
                     end loop;

                     if P_Index < Number_Of_Keys (P) then
                        Result := (True, P, P_Index, Scan, Start, Finish,
                                   Start_Interval, Finish_Interval);
                        if Match (Result) then
                           return Result;
                        else
                           return Empty_Mark;
                        end if;
                     else
                        return Empty_Mark;
                     end if;
                  end;
               else
                  return Empty_Mark;
               end if;
            else
               return Do_Search (Get_Child (Page, Index));
            end if;
         end if;
      end Do_Search;

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
      use type Marlowe.Btree.Slot;
      Page        : Btree_Page;
   begin

      Page := Get_Root_Page;

      while not Is_Leaf (Page) loop
         if Scan = Forward then
            Page := Get_Child (Page, 1);
         else
            Page := Get_Child (Page, Number_Of_Keys (Page) + 1);
         end if;
      end loop;

      if Scan = Forward then
         return (False, Page, 1, Scan);
      else
         return (False, Page, Number_Of_Keys (Page), Scan);
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
      use Pages;
   begin
      return Current.Page /= Pages.Null_Page;
   end Valid;

   ----------------
   -- Dump_Btree --
   ----------------

   procedure Dump_Btree is
      use Pages;
      use Marlowe.Btree;
   begin
      Dump_Page (Get_Root_Page);
   end Dump_Btree;

   ---------------
   -- Dump_Page --
   ---------------

   procedure Dump_Page (Page : Pages.Btree_Page) is
      use Pages;
      use Marlowe.Btree;
   begin
      WL.Trace.New_Line;
      WL.Trace.Put_Line ("----------- Page" & Image (Get_Index (Page)) &
                         " ----------");
      WL.Trace.Put_Line ("Leaf          : " &
                         Boolean'Image (Is_Leaf (Page)));
      WL.Trace.Put_Line ("Number_Of_Keys:" &
                         Slot'Image (Number_Of_Keys (Page)));
      for I in 1 .. Number_Of_Keys (Page) loop
         WL.Trace.Put_Line (Slot'Image (I) & " ==>" &
                            --  Image (Get_Key (Page, I)) &
                            Image (Get_Child (Page, I)));
      end loop;
      if not Is_Leaf (Page) then
         WL.Trace.Put_Line (Slot'Image (Number_Of_Keys (Page) + 1) & " ==>" &
                            --  " <>" &
                            Image
                            (Get_Child (Page, Number_Of_Keys (Page) + 1)));
      end if;
      WL.Trace.New_Line;

      if not Is_Leaf (Page) then
         for I in 1 .. Number_Of_Keys (Page) + 1 loop
            Dump_Page (Get_Child (Page, I));
         end loop;
      end if;
   end Dump_Page;

end Marlowe.Keys;

