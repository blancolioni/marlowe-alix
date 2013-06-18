with Marlowe.Key_Storage;

package body Marlowe.Btree_Page_Handles is

   ----------------
   -- Delete_Key --
   ----------------

   procedure Delete_Key (Item  : Btree_Page_Handle;
                         Index : Slot_Index)
   is
   begin
      pragma Assert (not Is_Leaf (Item) or else
                     Index <= Number_Of_Keys (Item));
      pragma Assert (Index <= Number_Of_Keys (Item) + 1);

      Set_Dirty (Item);
      if Index <= Number_Of_Keys (Item) then
         for I in Slot_Index range Index .. Number_Of_Keys (Item) - 1 loop
            Item.Move_Key (I + 1, I);
            --  Set_Key (Item, I, Get_Key (Item, I + 1));
            if not Is_Leaf (Item) then
               Set_Child (Item, I,
                          File_And_Page'(Get_Child (Item, I + 1)));
            end if;
         end loop;
         if not Is_Leaf (Item) then
            Set_Child (Item, Number_Of_Keys (Item),
                       File_And_Page'(Get_Child (Item,
                                                 Number_Of_Keys (Item) + 1)));
         end if;
      end if;

      Set_Number_Of_Keys (Item, Number_Of_Keys (Item) - 1);

   end Delete_Key;

   ---------------
   -- Dump_Page --
   ---------------

   procedure Dump_Page (Page      : Btree_Page_Handles.Btree_Page_Handle;
                        To_Output : Boolean := False)
   is
      pragma Unreferenced (To_Output);
      use Btree_Page_Handles;

      procedure Trace (Text : String);

      -----------
      -- Trace --
      -----------

      procedure Trace (Text : String) is
         pragma Unreferenced (Text);
      begin
         null;
--
--           if To_Output then
--              Ada.Text_IO.Put_Line (Text);
--           else
--              Marlowe.Trace.Put_Line (Text);
--           end if;
      end Trace;

      use Marlowe.Key_Storage;
      K : System.Storage_Elements.Storage_Array
        (1 .. System.Storage_Elements.Storage_Count
           (Page.Get_Key_Length));
   begin
      Trace ("----------- " &
                         Image (Get_Location (Page)) &
                         " ----------");
      Trace ("Leaf          : " &
                         Boolean'Image (Is_Leaf (Page)));
      Trace ("Number_Of_Keys:" &
                         Slot_Index'Image (Number_Of_Keys (Page)));
      for I in Slot_Index range 1 .. Number_Of_Keys (Page) loop
         Page.Get_Key (I, K);
         if not Is_Leaf (Page) then
            Trace (Slot_Index'Image (I) & " ==> " &
                               Image (K) & "  " &
                               Image (Get_Child (Page, I)));
         else
            Trace (Slot_Index'Image (I) & " ==> " &
                               Image (K));
         end if;
      end loop;
      if not Is_Leaf (Page) then
         Trace (Slot_Index'Image (Number_Of_Keys (Page) + 1) &
                            " ==>" &
                            " <>  " &
                            Image
                            (Get_Child (Page,
                                        Number_Of_Keys (Page) + 1)));
      end if;
--        Marlowe.Trace.Put_Line ("");

   end Dump_Page;

   --------------
   -- Find_Key --
   --------------

   function Find_Key (Item       : Btree_Page_Handle;
                      Key        : System.Storage_Elements.Storage_Array;
                      Forward    : Boolean)
                     return Slot_Index
   is
      use type System.Storage_Elements.Storage_Array;
      Current : System.Storage_Elements.Storage_Array
        (1 .. System.Storage_Elements.Storage_Offset (Get_Key_Length (Item)));
   begin

      if Forward then
         return Marlowe.Pages.Btree.Find_Key_Forward
           (Item.The_Btree_Page, Key);

--           for I in Slot_Index range 1 .. Number_Of_Keys (Item) loop
--              Get_Key (Item, I, Current);
--              if Current >= Key then
--                 return I;
--              end if;
--           end loop;
--           return Number_Of_Keys (Item) + 1;
      else
         for I in reverse Slot_Index range 1 .. Number_Of_Keys (Item) loop
            Get_Key (Item, I, Current);
            if Current = Key then
               return I;
            elsif Current < Key then
               return I + 1;
            end if;

--              Cmp := Compare (Definition, Current, Key);
--              case Cmp is
--                 when Equal =>
--                    return I;
--                 when Less =>
--                    return I + 1;
--                 when Greater =>
--                    null;
--              end case;
         end loop;
         return 1;
      end if;

   end Find_Key;

   --------------------
   -- Get_Btree_Page --
   --------------------

   function Get_Btree_Page
     (Handle : Btree_Page_Handle)
     return Marlowe.Pages.Btree.Btree_Page
   is
   begin
      return Handle.The_Btree_Page;
   end Get_Btree_Page;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Item  : Btree_Page_Handle;
                       Index : Slot_Index)
                      return File_And_Page
   is
   begin
      return Pages.Btree.Get_Child (Item.The_Btree_Page, Index);
   end Get_Child;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Item  : Btree_Page_Handle;
                       Index : Slot_Index)
                      return Btree_Page_Handle
   is
      Child  : constant File_And_Page := Get_Child (Item, Index);
      Result : Btree_Page_Handle;
   begin
      Derive (Item, Result);
      Set_Page (Result, Child);
      return Result;
   end Get_Child;

   -------------
   -- Get_Key --
   -------------

   --  function Get_Key (Item  : Btree_Page_Handle;
   --                    Index : Slot_Index)
   --                   return System.Storage_Elements.Storage_Array
   --  is
   --  begin
   --     return Pages.Btree.Get_Key (Item.The_Btree_Page, Index);
   --  end Get_Key;

   -------------
   -- Get_Key --
   -------------

   procedure Get_Key (Item   : in     Btree_Page_Handle;
                      Index  : in     Slot_Index;
                      Result :    out System.Storage_Elements.Storage_Array)
   is
   begin
      Pages.Btree.Get_Key (Item.The_Btree_Page, Index, Result);
   end Get_Key;

   --------------------
   -- Get_Key_Length --
   --------------------

   function Get_Key_Length (Item : in Btree_Page_Handle) return Positive is
   begin
      return Pages.Btree.Get_Key_Length (Item.The_Btree_Page);
   end Get_Key_Length;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Item : Btree_Page_Handle) return File_And_Page is
   begin
      return Pages.Btree.Get_Parent (Item.The_Btree_Page);
   end Get_Parent;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Item  : Btree_Page_Handle)
                       return Btree_Page_Handle
   is
      Parent : constant File_And_Page := Get_Parent (Item);
      Result : Btree_Page_Handle;
   begin
      Derive (Item, Result);
      Set_Page (Result, Parent);
      return Result;
   end Get_Parent;

   ----------------
   -- Insert_Key --
   ----------------

   procedure Insert_Key (Item       : Btree_Page_Handle;
                         Key        : System.Storage_Elements.Storage_Array;
                         Child      : File_And_Page)
   is
   begin
      pragma Assert (not Is_Leaf (Item) or Child = 0);

      Set_Dirty (Item);
      Set_Number_Of_Keys (Item, Number_Of_Keys (Item) + 1);

      if not Is_Leaf (Item) then
         Set_Child (Item, Number_Of_Keys (Item) + 1,
                    File_And_Page'(Get_Child (Item, Number_Of_Keys (Item))));
      end if;

      for I in reverse Slot_Index range 1 .. Number_Of_Keys (Item) - 1 loop
         declare
            Current : System.Storage_Elements.Storage_Array
              (1 .. System.Storage_Elements.Storage_Offset
               (Get_Key_Length (Item)));
            use type System.Storage_Elements.Storage_Array;
--              Current : constant System.Storage_Elements.Storage_Array :=
--                Get_Key (Item, I);
--              Cmp     : constant Compare_Result  :=
--                Compare (Definition, Current, Key);
         begin
            Get_Key (Item, I, Current);
            if Current < Key then
               Set_Key (Item, I + 1, Key);
               if not Is_Leaf (Item) then
                  Set_Child (Item, I + 1, Child);
               end if;
               return;
            else
               Item.Move_Key (I, I + 1);
               --  Set_Key (Item, I + 1, Get_Key (Item, I));
               if not Is_Leaf (Item) then
                  Set_Child (Item, I + 1,
                             File_And_Page'(Get_Child (Item, I)));
               end if;
            end if;
         end;
      end loop;
      Set_Key (Item, 1, Key);

      if not Is_Leaf (Item) then
         Set_Child (Item, 1, Child);
      end if;
   end Insert_Key;

   ----------------
   -- Insert_Key --
   ----------------

   procedure Insert_Key (Item       : Btree_Page_Handle;
                         Key        : System.Storage_Elements.Storage_Array)
   is
   begin
      pragma Assert (Is_Leaf (Item));
      Insert_Key (Item, Key, 0);
   end Insert_Key;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Item : Btree_Page_Handle) return Boolean is
   begin
      return Pages.Btree.Is_Leaf (Item.The_Btree_Page);
   end Is_Leaf;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Item : Btree_Page_Handle) return Boolean is
   begin
      return Get_Parent (Item) = 0;
   end Is_Root;

   ------------------
   -- Maximum_Keys --
   ------------------

   function Maximum_Keys (Item : in Btree_Page_Handle) return Slot_Index is
   begin
      return 2 * Minimum_Degree (Item) - 1;
   end Maximum_Keys;

   --------------------
   -- Minimum_Degree --
   --------------------

   function Minimum_Degree (Item  : Btree_Page_Handle) return Slot_Index is
   begin
      return Pages.Btree.Minimum_Degree (Item.The_Btree_Page);
   end Minimum_Degree;

   ------------------
   -- Minimum_Keys --
   ------------------

   function Minimum_Keys (Item : in Btree_Page_Handle) return Slot_Index is
   begin
      return Minimum_Degree (Item) - 1;
   end Minimum_Keys;

   --------------
   -- Move_Key --
   --------------

   procedure Move_Key (Item       : Btree_Page_Handle;
                       From_Index : Slot_Index;
                       To_Index   : Slot_Index)
   is
      K : System.Storage_Elements.Storage_Array
        (1 .. System.Storage_Elements.Storage_Count (Item.Get_Key_Length));
   begin
      Item.Get_Key (From_Index, K);
      Item.Set_Key (To_Index, K);
   end Move_Key;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (Handle     : in out Btree_Page_Handle;
                       Reference  : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.New_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Btree_Page :=
        Marlowe.Pages.Btree.To_Btree_Page (Get_Page (Handle));
      Marlowe.Pages.Btree.Initialise (Handle.The_Btree_Page);
      Set_Dirty (Handle);
   end New_Page;

   --------------------
   -- Number_Of_Keys --
   --------------------

   function Number_Of_Keys (Item : in Btree_Page_Handle) return Slot_Index is
   begin
      return Pages.Btree.Number_Of_Keys (Item.The_Btree_Page);
   end Number_Of_Keys;

   ----------------
   -- Set_Child --
   ----------------

   procedure Set_Child (Item  : Btree_Page_Handle;
                        Index : Slot_Index;
                        Child : File_And_Page)
   is
   begin
      Pages.Btree.Set_Child (Item.The_Btree_Page, Index, Child);
   end Set_Child;

   ----------------
   -- Set_Child --
   ----------------

   procedure Set_Child (Item  : Btree_Page_Handle;
                        Index : Slot_Index;
                        Child : Btree_Page_Handle)
   is
   begin
      Set_Child (Item, Index, Get_Location (Child));
      Set_Parent (Child, Get_Location (Item));
      Set_Dirty (Child);
   end Set_Child;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (Item  : Btree_Page_Handle;
                      Index : Slot_Index;
                      Key   : System.Storage_Elements.Storage_Array)
   is
   begin
      Pages.Btree.Set_Key (Item.The_Btree_Page, Index, Key);
   end Set_Key;

   ------------------------
   -- Set_Number_Of_Keys --
   ------------------------

   procedure Set_Number_Of_Keys (Item : Btree_Page_Handle;
                                 Num  : Slot_Index)
   is
   begin
      Pages.Btree.Set_Number_Of_Keys (Item.The_Btree_Page, Num);
   end Set_Number_Of_Keys;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page (Handle    : in out Btree_Page_Handle;
                       Reference : in     File_And_Page)
   is
   begin
      Marlowe.Page_Handles.Set_Page
        (Handle    => Marlowe.Page_Handles.Page_Handle (Handle),
         Reference => Reference);
      Handle.The_Btree_Page :=
        Marlowe.Pages.Btree.To_Btree_Page (Get_Page (Handle));
   end Set_Page;

   -------------------
   -- Set_Page_Info --
   -------------------

   procedure Set_Page_Info
     (Item       : in Btree_Page_Handle;
      Leaf       : in Boolean;
      Key_Length : in Positive)
   is
   begin
      Marlowe.Pages.Btree.Set_Page_Info (Item.The_Btree_Page, Leaf,
                                         Key_Length);
      Set_Dirty (Item);
   end Set_Page_Info;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (Item   : Btree_Page_Handle;
                         Parent : File_And_Page)
   is
   begin
      Set_Dirty (Item);
      Pages.Btree.Set_Parent (Item.The_Btree_Page, Parent);
   end Set_Parent;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (Item   : Btree_Page_Handle;
                         Parent : Btree_Page_Handle)
   is
   begin
      Set_Parent (Item, Get_Location (Parent));
   end Set_Parent;

   -----------
   -- Split --
   -----------

   procedure Split (Source   : in Btree_Page_Handle;
                    New_Page : in Btree_Page_Handle;
                    Index    : in Slot_Index;
                    Parent   : in Btree_Page_Handle)
   is
      K : System.Storage_Elements.Storage_Array
        (1 .. System.Storage_Elements.Storage_Count (Source.Get_Key_Length));
      Min_Degree : constant Slot_Index := Source.Minimum_Degree;
   begin
      pragma Assert (Number_Of_Keys (Source) = Maximum_Keys (Source));
      pragma Assert (Number_Of_Keys (New_Page) = 0);
      pragma Assert (Get_Location (New_Page) /= 0);
      pragma Assert (Number_Of_Keys (Parent) < Maximum_Keys (Parent));
      pragma Assert (Is_Leaf (New_Page) = Is_Leaf (Source));

      Set_Dirty (Source);
      Set_Dirty (New_Page);
      Set_Dirty (Parent);

      Set_Number_Of_Keys (New_Page, Minimum_Keys (New_Page));

      for I in 1 .. Minimum_Keys (New_Page) loop
         Source.Get_Key (I + Min_Degree, K);
         New_Page.Set_Key (I, K);

         --  Set_Key (New_Page, I,
         --     Get_Key (Source, I + Minimum_Degree (Source)));
      end loop;

      if not Is_Leaf (New_Page) then
         for I in 1 .. Minimum_Keys (New_Page) + 1 loop
            declare
               Child : constant Btree_Page_Handle :=
                 Get_Child (Source, I + Minimum_Degree (Source));
            begin
               Set_Child (New_Page, I, Child);
            end;
         end loop;
      end if;

      Set_Number_Of_Keys (Parent, Number_Of_Keys (Parent) + 1);

      for I in reverse Index .. Number_Of_Keys (Parent) - 1 loop
         Parent.Move_Key (I, I + 1);
         --  Set_Key (Parent, I + 1, Get_Key (Parent, I));
      end loop;

      for I in reverse Index + 1 .. Number_Of_Keys (Parent) loop
         Set_Child (Parent, I + 1,
                    File_And_Page'(Get_Child (Parent, I)));
      end loop;

      Set_Child (Parent, Index + 1, Get_Location (New_Page));
      Source.Get_Key (Min_Degree, K);
      Parent.Set_Key (Index, K);

      --  Set_Key (Parent, Index, Get_Key (Source, Minimum_Degree (Source)));

      Set_Number_Of_Keys (Source, Minimum_Keys (Source));

      Set_Parent (New_Page, Parent);

   end Split;

end Marlowe.Btree_Page_Handles;
