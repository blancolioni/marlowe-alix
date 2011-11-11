with Ada.Strings.Unbounded;

package body Marlowe.Btree_Keys is

   function Compare_Raw
     (Left, Right : System.Storage_Elements.Storage_Array)
     return Compare_Result;
   pragma Inline (Compare_Raw);

   package body Unsigned_Key_Components is

      -----------------
      -- To_Ada_Type --
      -----------------

      function To_Ada_Type
        (Component : Key_Component;
         Item      : System.Storage_Elements.Storage_Array)
        return Component_Type
      is
         use type System.Storage_Elements.Storage_Offset;
         Result : Component_Type := 0;
      begin
         pragma Assert (Item'Length =
                          System.Storage_Elements.Storage_Offset
                          (Component.Length));
         for I in Item'Range loop
            Result := Result * (2 ** System.Storage_Unit) +
              Component_Type (Item (I));
         end loop;
         return Result;
      end To_Ada_Type;

      ------------------
      -- To_Btree_Key --
      ------------------

      function To_Btree_Key
        (Component : Key_Component;
         Item      : Component_Type)
        return System.Storage_Elements.Storage_Array
      is
         use System.Storage_Elements;
         Result : System.Storage_Elements.Storage_Array
           (1 .. Storage_Count (Component.Length));
         It     : Component_Type := Item;
      begin
         if Component.Length > 1 then
            for I in reverse Result'Range loop
               Result (I) := Storage_Element (It mod (2**System.Storage_Unit));
               It := It / (2**System.Storage_Unit);
            end loop;
         else
            Result (1) := Storage_Element (Item);
         end if;

         return Result;
      end To_Btree_Key;

   end Unsigned_Key_Components;


   package body Signed_Key_Components is

      -----------------
      -- To_Ada_Type --
      -----------------

      function To_Ada_Type
        (Component : Key_Component;
         Item      : System.Storage_Elements.Storage_Array)
        return Component_Type
      is
         use type System.Storage_Elements.Storage_Element;
         use type System.Storage_Elements.Storage_Offset;
         Result : Component_Type;
         MSB    : constant Boolean :=
           Item (Item'First) / (2**(System.Storage_Unit - 1)) /= 0;
      begin
         pragma Assert (Item'Length =
                          System.Storage_Elements.Storage_Offset
                          (Component.Length));
         for I in Item'Range loop
            if I = Item'First then
               Result := Component_Type (Item (Item'First) and not
                                    (2**(System.Storage_Unit - 1)));
            else
               Result :=
                 Result * (2**System.Storage_Unit) +
                 Component_Type (Item (I));
            end if;
         end loop;
         if not MSB then
            Result := -(Component_Type'Base'Last / 2 + 1 - Result);
         end if;
         return Result;
      end To_Ada_Type;


      ------------------
      -- To_Btree_Key --
      ------------------

      function To_Btree_Key
        (Component : Key_Component;
         Item      : Component_Type)
        return System.Storage_Elements.Storage_Array
      is
         use System.Storage_Elements;
         Result : System.Storage_Elements.Storage_Array
           (1 .. Storage_Count (Component.Length));
         It       : Component_Type'Base := Item;
         Negative : constant Boolean := Item < 0;
      begin
         if It < 0 then
            It := It + Component_Type'Base'Last / 2 + 1;
         end if;
         if Component.Length > 1 then
            for I in reverse Result'Range loop
               Result (I) := Storage_Element (It mod (2**System.Storage_Unit));
               It := It / (2**System.Storage_Unit);
            end loop;
         else
            Result (1) := Storage_Element (It);
         end if;
         if not Negative then
            Result (Result'First) :=
              Result (Result'First) or (2**(System.Storage_Unit - 1));
         end if;
         return Result;
      end To_Btree_Key;

   end Signed_Key_Components;

   package body Discrete_Key_Components is

      type Temporary is new Integer;
      --  ouch!
      --  How do I use a type that's the same size as
      --  Component_Type?

      -----------------
      -- To_Ada_Type --
      -----------------

      function To_Ada_Type
        (Component : Key_Component;
         Item      : System.Storage_Elements.Storage_Array)
        return Component_Type
      is
         use type System.Storage_Elements.Storage_Element;
         use type System.Storage_Elements.Storage_Offset;
         Result : Temporary;
         MSB    : constant Boolean :=
           Item (Item'First) / (2**(System.Storage_Unit - 1)) /= 0;
      begin
         pragma Assert (Item'Length =
                          System.Storage_Elements.Storage_Offset
                          (Component.Length));
         for I in Item'Range loop
            if I = Item'First then
               Result := Temporary (Item (Item'First) and not
                                    (2**(System.Storage_Unit - 1)));
            else
               Result :=
                 Result * (2**System.Storage_Unit) +
                 Temporary (Item (I));
            end if;
         end loop;
         if not MSB then
            Result := -(Temporary'Base'Last / 2 + 1 - Result);
         end if;
         return Component_Type'Val (Result);
      end To_Ada_Type;


      ------------------
      -- To_Btree_Key --
      ------------------

      function To_Btree_Key
        (Component : Key_Component;
         Item      : Component_Type)
        return System.Storage_Elements.Storage_Array
      is
         use System.Storage_Elements;
         Result : System.Storage_Elements.Storage_Array
           (1 .. Storage_Count (Component.Length));
         It       : Temporary := Component_Type'Pos (Item);
         Negative : constant Boolean := Component_Type'Pos (Item) < 0;
      begin
         if It < 0 then
            It := It + Temporary'Base'Last / 2 + 1;
         end if;
         for I in reverse Result'Range loop
            Result (I) := Storage_Element (It mod (2**System.Storage_Unit));
            It := It / (2**System.Storage_Unit);
         end loop;
         if not Negative then
            Result (Result'First) :=
              Result (Result'First) or (2**(System.Storage_Unit - 1));
         end if;
         return Result;
      end To_Btree_Key;

   end Discrete_Key_Components;

   package Integer_Component is
      new Signed_Key_Components (Integer);

   package Record_Index_Component is
     new Unsigned_Key_Components (Database_Index);

   -------------
   -- Compare --
   -------------

   function Compare (Using_Key   : Component_Array;
                     Left, Right : System.Storage_Elements.Storage_Array)
                    return Compare_Result
   is
      pragma Unreferenced (Using_Key);
   begin
      return Compare_Raw (Left, Right);
   end Compare;

   -----------------
   -- Compare_Raw --
   -----------------

   function Compare_Raw
     (Left, Right : System.Storage_Elements.Storage_Array)
     return Compare_Result
   is
      use type System.Storage_Elements.Storage_Element;
      use type System.Storage_Elements.Storage_Offset;
      use type System.Storage_Elements.Storage_Array;
   begin
      if Left < Right then
         return Less;
      elsif Left = Right then
           return Equal;
      else
         return Greater;
      end if;

   end Compare_Raw;

   -----------
   -- Equal --
   -----------

   function Equal (Using_Key   : Component_Array;
                   Left, Right : System.Storage_Elements.Storage_Array)
                  return Boolean
   is
      pragma Unreferenced (Using_Key);
      use type System.Storage_Elements.Storage_Array;
   begin
      return Left = Right;
   end Equal;

   ------------------
   -- Get_Key_Type --
   ------------------

   function Get_Key_Type (From : Key_Component) return Btree_Key_Type is
   begin
      return From.Key_Type;
   end Get_Key_Type;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length   (From : Key_Component) return Natural is
   begin
      return Natural (From.Length);
   end Get_Length;

   -----------
   -- Image --
   -----------

   function Image (Item : System.Storage_Elements.Storage_Array)
                  return String
   is
      use Ada.Strings.Unbounded;
      use System.Storage_Elements;
      Hex_Digits : constant String := "0123456789ABCDEF";
      Result : Unbounded_String;
   begin
      for I in Item'Range loop
         Result := Result &
           Hex_Digits (Natural (Item (I)) / 16 + 1) &
           Hex_Digits (Natural (Item (I)) mod 16 + 1);
      end loop;
      return To_String (Result);
   end Image;

   --------------------
   -- Make_Component --
   --------------------

   function Make_Component (Key_Type : Btree_Key_Type;
                            Length   : Natural)
                           return Key_Component
   is
   begin
      return (Key_Component_Length (Length), False, Key_Type);
   end Make_Component;

   ------------------
   -- To_Btree_Key --
   ------------------

   function To_Btree_Key (Component : Key_Component;
                          Item      : Integer)
                         return System.Storage_Elements.Storage_Array
     renames Integer_Component.To_Btree_Key;

   ------------------
   -- To_Btree_Key --
   ------------------

   function To_Btree_Key (Item      : Database_Index)
                          return System.Storage_Elements.Storage_Array
   is
   begin
      return Record_Index_Component.To_Btree_Key
        ((8, False, Unsigned_Integer_Key),
         Item);
   end To_Btree_Key;

   ------------------
   -- To_Btree_Key --
   ------------------

   function To_Btree_Key (Component : Key_Component;
                          Key       : String)
                         return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Result : Storage_Array (1 .. Storage_Count (Component.Length)) :=
        (others => 0);
   begin
      pragma Assert (Key'Length <= Natural (Component.Length - 2));
      Result (Result'Last - 1) :=
        Storage_Element (Key'Length / (2**System.Storage_Unit));
      Result (Result'Last) :=
        Storage_Element (Key'Length mod (2**System.Storage_Unit));
      for I in Key'Range loop
         Result (Storage_Offset (I - Key'First) +  Result'First) :=
           Character'Pos (Key (I));
      end loop;
      return Result;
   end To_Btree_Key;

   -----------------------
   -- To_Database_Index --
   -----------------------

   function To_Database_Index (Key : System.Storage_Elements.Storage_Array)
                               return Database_Index
   is
   begin
      return Record_Index_Component.To_Ada_Type
        ((8, False, Unsigned_Integer_Key),
         Key);
   end To_Database_Index;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Component : Key_Component;
      Item      : System.Storage_Elements.Storage_Array)
     return Integer
     renames Integer_Component.To_Ada_Type;

   ---------------
   -- To_String --
   ---------------

   function To_String (Component : Key_Component;
                       Item      : System.Storage_Elements.Storage_Array)
                      return String
   is
      use System.Storage_Elements;
      Length : constant Storage_Count :=
        Storage_Count (Item (Item'Last - 1)) * (2**System.Storage_Unit) +
        Storage_Count (Item (Item'Last));
      Result : String (1 .. Natural (Length));
   begin
      pragma Assert (Item'Length <= Storage_Offset (Component.Length));
      for I in Result'Range loop
         Result (I) := Character'Val (Item (Storage_Count (I) +
                                            Item'First - 1));
      end loop;
      return Result;
   end To_String;

end Marlowe.Btree_Keys;
