with Ada.Unchecked_Deallocation;

package body Marlowe.Btree.Cache is

   Cache_Block_Size : constant := 4096;

   subtype Cache_Block_Index is Page_Index range 0 .. Cache_Block_Size - 1;

   type Cache_Block_Record (Leaf : Boolean := True);
   type Cache_Block is access Cache_Block_Record;

   type Array_Of_Sub_Blocks is array (Cache_Block_Index) of Cache_Block;
   type Array_Of_Elements   is array (Cache_Block_Index) of Element_Type;

   type Cache_Block_Record (Leaf : Boolean := True) is
      record
         Count : Natural     := 0;
         Next  : Cache_Block := null;
         case Leaf is
            when True =>
               Elements : Array_Of_Elements := (others => Null_Element);
            when False =>
               Divider    : Page_Index := 0;
               Sub_Blocks : Array_Of_Sub_Blocks := (others => null);
         end case;
      end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Cache_Block_Record, Cache_Block);

   The_Cache : Cache_Block := null;
   Max_Index : Page_Index  := 0;
   Depth     : Natural     := 0;

   Free_Blocks : array (Boolean) of Cache_Block;

   procedure Ensure_Size (Index : Page_Index);
   --  Ensure that the cache can hold the given page index

   function Allocate_Block (Leaf : Boolean) return Cache_Block;
   --  Allocate a block, either from memory or from the list of
   --  free blocks.

   --------------------
   -- Allocate_Block --
   --------------------

   function Allocate_Block (Leaf : Boolean) return Cache_Block is
      Result : Cache_Block;
   begin
      if Free_Blocks (Leaf) = null then
         Result := new Cache_Block_Record (Leaf);
      else
         Result := Free_Blocks (Leaf);
         Free_Blocks (Leaf) := Free_Blocks (Leaf).Next;
         Result.Next := null;
      end if;
      return Result;
   end Allocate_Block;

   -----------
   -- Close --
   -----------

   procedure Close is

      procedure Free_Cache (Start : in out Cache_Block);
      procedure Free_Block_List (List : in out Cache_Block);

      ---------------------
      -- Free_Block_List --
      ---------------------

      procedure Free_Block_List (List : in out Cache_Block) is
         Tmp : Cache_Block;
      begin
         while List /= null loop
            Tmp := List;
            List := List.Next;
            Free (Tmp);
         end loop;
      end Free_Block_List;

      ----------------
      -- Free_Cache --
      ----------------

      procedure Free_Cache (Start : in out Cache_Block) is
      begin
         if Start /= null then
            if not Start.Leaf then
               for I in Start.Sub_Blocks'Range loop
                  Free_Cache (Start.Sub_Blocks (I));
               end loop;
            end if;
            Free (Start);
         end if;
      end Free_Cache;

   begin
      Free_Cache (The_Cache);
      for I in Free_Blocks'Range loop
         Free_Block_List (Free_Blocks (I));
      end loop;
      The_Cache := null;
      Depth     := 0;
      Max_Index := 0;
      Free_Blocks := (others => null);
   end Close;

   -----------------
   -- Ensure_Size --
   -----------------

   procedure Ensure_Size (Index : Page_Index) is
   begin
      while Max_Index < Index loop
         if The_Cache = null then
            The_Cache := Allocate_Block (True);
            Max_Index := Cache_Block_Size - 1;
            Depth := 1;
         else
            declare
               New_Cache : constant Cache_Block :=
                 Allocate_Block (False);
            begin
               New_Cache.Count := 1;
               New_Cache.Divider := Max_Index + 1;
               New_Cache.Sub_Blocks (0) := The_Cache;
               The_Cache := New_Cache;
            end;
            Max_Index := Cache_Block_Size * (Max_Index + 1) - 1;
            Depth := Depth + 1;
         end if;
      end loop;
   end Ensure_Size;

   ---------------------------
   -- For_All_Cache_Entries --
   ---------------------------

   procedure For_All_Cache_Entries (Do_This : Action) is

      procedure FACE (Block : Cache_Block);

      ----------
      -- FACE --
      ----------

      procedure FACE (Block : Cache_Block) is
      begin
         if Block = null then
            return;
         elsif Block.Leaf then
            for I in Block.Elements'Range loop
               if Block.Elements (I) /= Null_Element then
                  Do_This (Block.Elements (I));
               end if;
            end loop;
         else
            for I in Block.Sub_Blocks'Range loop
               if Block.Sub_Blocks (I) /= null then
                  FACE (Block.Sub_Blocks (I));
               end if;
            end loop;
         end if;
      end FACE;

   begin
      FACE (The_Cache);
   end For_All_Cache_Entries;

   ---------------------
   -- Get_Cache_Entry --
   ---------------------

   function  Get_Cache_Entry (Index : Page_Index) return Element_Type is
      Block     : Cache_Block := The_Cache;
      Sub_Index : Page_Index  := Index;
   begin

      if Index > Max_Index then
         return Null_Element;
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
         return Block.Elements (Sub_Index);
      else
         return Null_Element;
      end if;
   end Get_Cache_Entry;

   ------------------------
   -- Insert_Cache_Entry --
   ------------------------

   procedure Insert_Cache_Entry (Element : Element_Type) is
      Block     : Cache_Block;
      Sub_Index : Page_Index := Get_Page_Index (Element);
   begin
      Ensure_Size (Sub_Index);
      Block := The_Cache;
      while not Block.Leaf loop
         declare
            Divider : constant Page_Index := Block.Divider;
         begin
            if Block.Sub_Blocks (Sub_Index / Divider) = null then
               if Divider = Cache_Block_Size then
                  Block.Sub_Blocks (Sub_Index / Divider) :=
                    Allocate_Block (True);
               else
                  Block.Sub_Blocks (Sub_Index / Divider) :=
                    Allocate_Block (False);
                  Block.Sub_Blocks (Sub_Index / Divider).Divider :=
                    Divider / Cache_Block_Size;
               end if;
               Block.Count := Block.Count + 1;
            end if;
            Block := Block.Sub_Blocks (Sub_Index / Divider);
            Sub_Index := Sub_Index mod Divider;
         end;
      end loop;

      pragma Assert (Block.Elements (Sub_Index) = Null_Element);
      Block.Elements (Sub_Index) := Element;
      Block.Count := Block.Count + 1;
   end Insert_Cache_Entry;

   ------------------------
   -- Remove_Cache_Entry --
   ------------------------

   procedure Remove_Cache_Entry (Index : Page_Index) is
      Block      : Cache_Block := The_Cache;
      Sub_Index  : Page_Index  := Index;
      Path       : array (1 .. Depth) of Cache_Block;
      Index_Path : array (1 .. Depth) of Page_Index;
      Path_Count : Natural := 0;
   begin
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

      Block.Elements (Sub_Index) := Null_Element;
      Block.Count := Block.Count - 1;

      while Path_Count > 0 and Block.Count = 0 loop
         Block.Next := Free_Blocks (Block.Leaf);
         Free_Blocks (Block.Leaf) := Block;
         Block := Path (Path_Count);
         Block.Sub_Blocks (Index_Path (Path_Count)) := null;
         Block.Count := Block.Count - 1;
         Path_Count := Path_Count - 1;
      end loop;

   end Remove_Cache_Entry;

end Marlowe.Btree.Cache;


