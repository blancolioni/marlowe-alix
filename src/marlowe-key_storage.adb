with Ada.Unchecked_Conversion;
with System;

package body Marlowe.Key_Storage is

   type Float_Integer is mod 2 ** Float'Size;
   type Long_Float_Integer is mod 2 ** Long_Float'Size;

   function To_Float_Integer is
     new Ada.Unchecked_Conversion (Float, Float_Integer);

   function To_Long_Float_Integer is
     new Ada.Unchecked_Conversion (Long_Float, Long_Float_Integer);

   function To_Float is
     new Ada.Unchecked_Conversion (Float_Integer, Float);

   function To_Long_Float is
     new Ada.Unchecked_Conversion (Long_Float_Integer, Long_Float);

   generic
      type Storage_Type is mod <>;
   package Unsigned_Storage is
      procedure To_Storage
        (Value   : in     Storage_Type;
         Storage : in out Storage_Array);

      procedure From_Storage
        (Value   :    out Storage_Type;
         Storage : in     Storage_Array);
   end Unsigned_Storage;

   package body Integral_Storage is

      type Unsigned_Integral is mod System.Max_Binary_Modulus;

      package Unsigned_Integral_Storage is
        new Unsigned_Storage (Unsigned_Integral);

      ------------------
      -- From_Storage --
      ------------------

      procedure From_Storage
        (Value   :    out Integral;
         Storage : in     Storage_Array)
      is
         X : Unsigned_Integral;
      begin
         Unsigned_Integral_Storage.From_Storage (X, Storage);

         if X = 0 then
            Value := Integral'First;
         elsif X >= Unsigned_Integral (Integral'Last) then
            Value := Integral (X - Unsigned_Integral (Integral'Last));
         else
            Value := Integral (X) - Integral'Last;
         end if;
      end From_Storage;

      ----------------
      -- To_Storage --
      ----------------

      procedure To_Storage
        (Value   : in     Integral;
         Storage : in out Storage_Array)
      is
         X      : Unsigned_Integral;
      begin
         if Value = Integral'First then
            X := 0;
         elsif Value < 0 then
            X := Unsigned_Integral (Value + Integral'Last);
         else
            X := Unsigned_Integral (Value) + Unsigned_Integral (Integral'Last);
         end if;

         Unsigned_Integral_Storage.To_Storage (X, Storage);
      end To_Storage;

   end Integral_Storage;

   ----------------------
   -- Unsigned_Storage --
   ----------------------

   package body Unsigned_Storage is

      ------------------
      -- From_Storage --
      ------------------

      procedure From_Storage
        (Value   :    out Storage_Type;
         Storage : in     Storage_Array)
      is
      begin
         Value := 0;
         for I in Storage'Range loop
            Value := Value * 256 + Storage_Type (Storage (I));
         end loop;
      end From_Storage;

      ----------------
      -- To_Storage --
      ----------------

      procedure To_Storage
        (Value   : in     Storage_Type;
         Storage : in out Storage_Array)
      is
         T : Storage_Type := Value;
      begin
         for I in reverse Storage'Range loop
            Storage (I) :=
              System.Storage_Elements.Storage_Element
                (T mod 256);
            T := T / 256;
         end loop;
      end To_Storage;

   end Unsigned_Storage;

   package Database_Index_Storage is
     new Unsigned_Storage (Database_Index);

   package Unsigned_Integer_Storage is
     new Unsigned_Storage (Unsigned_Integer);

   package Float_Storage is
     new Unsigned_Storage (Float_Integer);

   package Long_Float_Storage is
     new Unsigned_Storage (Long_Float_Integer);

   package body Unsigned_Key_Components is

      -----------------------
      -- To_Database_Index --
      -----------------------

      function To_Ada_Type
        (Key_Value : System.Storage_Elements.Storage_Array)
      return Component_Type
      is
         Result : Component_Type := 0;
      begin
         for I in Key_Value'Range loop
            Result := Result * 256 + Component_Type (Key_Value (I));
         end loop;
         return Result;
      end To_Ada_Type;

      ----------------------
      -- To_Storage_Array --
      ----------------------

      function To_Storage_Array
        (Value : Component_Type;
         Length : System.Storage_Elements.Storage_Count)
         return System.Storage_Elements.Storage_Array
      is
         use System.Storage_Elements;
         Result : Storage_Array (1 .. Length);
         X      : Component_Type := Value;
      begin
         for I in reverse Result'Range loop
            Result (I) := Storage_Element (X mod 256);
            X := X / 256;
         end loop;
         return Result;
      end To_Storage_Array;

   end Unsigned_Key_Components;

   package Unsigned_Integer_Components is
     new Unsigned_Key_Components (Unsigned_Integer);

   package Float_Integer_Components is
     new Unsigned_Key_Components (Float_Integer);

   package Long_Float_Integer_Components is
     new Unsigned_Key_Components (Long_Float_Integer);

   ---------------------------------
   -- Bounded_String_From_Storage --
   ---------------------------------

   procedure Bounded_String_From_Storage
     (Value   :    out String;
      Length  :    out Natural;
      Storage : in     Storage_Array)
   is
      use type System.Storage_Elements.Storage_Element;
   begin
      Length := 0;
      for I in Storage'Range loop
         exit when Storage (I) = 0;
         Length := Length + 1;
         Value (Length) := Character'Val (Storage (I));
      end loop;
   end Bounded_String_From_Storage;

   -------------------------------
   -- Bounded_String_To_Storage --
   -------------------------------

   procedure Bounded_String_To_Storage
     (Value   : in     String;
      Storage : in out Storage_Array)
   is
      use System.Storage_Elements;
      Index : Positive := Value'First;
   begin
      for I in Storage'Range loop
         if Index in Value'Range then
            Storage (I) := Character'Pos (Value (Index));
            Index := Index + 1;
         else
            Storage (I) := 0;
         end if;
      end loop;
      Storage (Storage'Last - 1) :=
        Storage_Element (Value'Length / (2 ** System.Storage_Unit));
      Storage (Storage'Last) :=
        Storage_Element (Value'Length mod (2 ** System.Storage_Unit));
   end Bounded_String_To_Storage;

   -------------
   -- Compare --
   -------------

   function Compare
     (Left, Right : System.Storage_Elements.Storage_Array)
      return Compare_Result
   is
      use System.Storage_Elements;
      X, Y : Storage_Element;
   begin
      for I in Left'Range loop
         X := Left (I);
         if I - Left'First < Right'Length then
            Y := Right (I - Left'First + Right'First);
            if X < Y then
               return Less;
            elsif X > Y then
               return Greater;
            end if;
         end if;
      end loop;
      if Left'Length < Right'Length then
         return Less;
      elsif Left'Length = Right'Length then
         return Equal;
      else
         return Greater;
      end if;
   end Compare;

   -----------
   -- Equal --
   -----------

   function Equal
     (Left, Right : System.Storage_Elements.Storage_Array)
      return Boolean
   is
      use type System.Storage_Elements.Storage_Array;
   begin
      return Left = Right;
   end Equal;

   -------------------------------
   -- Fixed_String_From_Storage --
   -------------------------------

   procedure Fixed_String_From_Storage
     (Value   :    out String;
      Storage : in     Storage_Array)
   is
      Index : Natural := Value'First - 1;
   begin
      for Unit of Storage loop
         Index := Index + 1;
         Value (Index) := Character'Val (Unit);
      end loop;
   end Fixed_String_From_Storage;

   -----------------------------
   -- Fixed_String_To_Storage --
   -----------------------------

   procedure Fixed_String_To_Storage
     (Value   : in     String;
      Storage : in out Storage_Array)
   is
      Index : Natural := Value'First - 1;
   begin
      for Unit of Storage loop
         Index := Index + 1;
         Unit := Character'Pos (Value (Index));
      end loop;
   end Fixed_String_To_Storage;

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out Database_Index;
      Storage : in     Storage_Array)
   is
   begin
      Database_Index_Storage.From_Storage (Value, Storage);
   end From_Storage;

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out Boolean;
      Storage : in     Storage_Array)
   is
      use type System.Storage_Elements.Storage_Element;
   begin
      Value := Storage (Storage'First) /= 0;
   end From_Storage;

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out Float;
      Storage : in     Storage_Array)
   is
      X : Float_Integer;
   begin
      Float_Storage.From_Storage (X, Storage);
      if X < 2 ** (Float'Size - 1) then
         X := Float_Integer'Last - X;
      else
         X := X - 2 ** (Float'Size - 1);
      end if;
      Value := To_Float (X);
   end From_Storage;

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out Long_Float;
      Storage : in     Storage_Array)
   is
      X : Long_Float_Integer;
   begin
      Long_Float_Storage.From_Storage (X, Storage);
      if X < 2 ** (Long_Float'Size - 1) then
         X := Long_Float_Integer'Last - X;
      else
         X := X - 2 ** (Long_Float'Size - 1);
      end if;
      Value := To_Long_Float (X);
   end From_Storage;

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out Integer;
      Storage : in     Storage_Array)
   is
      X : Unsigned_Integer;
   begin
      Unsigned_Integer_Storage.From_Storage (X, Storage);
      if X = 0 then
         Value := Integer'First;
      elsif X >= Unsigned_Integer (Integer'Last) then
         Value := Integer (X - Unsigned_Integer (Integer'Last));
      else
         Value := Integer (X) - Integer'Last;
      end if;
   end From_Storage;

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out File_And_Page;
      Storage : in     Storage_Array)
   is
      X : Database_Index;
   begin
      From_Storage (X, Storage);
      Value := File_And_Page (X);
   end From_Storage;

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out Unsigned_Integer;
      Storage : in     Storage_Array)
   is
   begin
      Unsigned_Integer_Storage.From_Storage (Value, Storage);
   end From_Storage;

   -----------
   -- Image --
   -----------

   function Image
     (Key : System.Storage_Elements.Storage_Array)
      return String
   is
      Hex    : constant String := "0123456789ABCDEF";
      Result : String (1 .. Natural (Key'Length) * 3 - 1);
      X      : Natural := 0;
   begin
      for I in Key'Range loop
         X := X + 1;
         Result (X) := Hex (Natural (Key (I)) / 16 + 1);
         X := X + 1;
         Result (X) := Hex (Natural (Key (I)) mod 16 + 1);
         if X < Result'Last then
            X := X + 1;
            Result (X) := ':';
         end if;
      end loop;
      return Result;
   end Image;

   -----------------------
   -- To_Database_Index --
   -----------------------

   function To_Database_Index
     (Key_Value : System.Storage_Elements.Storage_Array)
      return Database_Index
   is
      use type System.Storage_Elements.Storage_Offset;
      Result : Database_Index := 0;
   begin
      for I in Key_Value'Last - 7 .. Key_Value'Last loop
         Result := Result * 256 + Database_Index (Key_Value (I));
      end loop;
      return Result;
   end To_Database_Index;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     Database_Index;
      Storage : in out Storage_Array)
   is
   begin
      Database_Index_Storage.To_Storage (Value, Storage);
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     File_And_Page;
      Storage : in out Storage_Array)
   is
   begin
      To_Storage (Database_Index (Value), Storage);
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     Boolean;
      Storage : in out Storage_Array)
   is
   begin
      Storage (Storage'First) := Boolean'Pos (Value);
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     Float;
      Storage : in out Storage_Array)
   is
      X : Float_Integer :=
            To_Float_Integer (Value);
   begin
      if X >= 2 ** (Float'Size - 1) then
         X := Float_Integer'Last - X;
      else
         X := X + 2 ** (Float'Size - 1);
      end if;
      Float_Storage.To_Storage (X, Storage);
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     Long_Float;
      Storage : in out Storage_Array)
   is
      X : Long_Float_Integer :=
            To_Long_Float_Integer (Value);
   begin
      if X >= 2 ** (Long_Float'Size - 1) then
         X := Long_Float_Integer'Last - X;
      else
         X := X + 2 ** (Long_Float'Size - 1);
      end if;
      Long_Float_Storage.To_Storage (X, Storage);
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     Integer;
      Storage : in out Storage_Array)
   is
      X      : Unsigned_Integer;
   begin
      if Value = Integer'First then
         X := 0;
      elsif Value < 0 then
         X := Unsigned_Integer (Value + Integer'Last);
      else
         X := Unsigned_Integer (Value) + Unsigned_Integer (Integer'Last);
      end if;

      Unsigned_Integer_Storage.To_Storage (X, Storage);
   end To_Storage;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     Unsigned_Integer;
      Storage : in out Storage_Array)
   is
   begin
      Unsigned_Integer_Storage.To_Storage (Value, Storage);
   end To_Storage;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Value : Database_Index)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Result : Storage_Array (1 .. 8);
      X      : Database_Index := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Storage_Element (X mod 256);
         X := X / 256;
      end loop;
      return Result;
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Value : Integer;
      Length : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Array
   is
      X      : Unsigned_Integer;
   begin
      if Value = Integer'First then
         X := 0;
      elsif Value < 0 then
         X := Unsigned_Integer (Value + Integer'Last);
      else
         X := Unsigned_Integer (Value) + Unsigned_Integer (Integer'Last);
      end if;

      return Unsigned_Integer_Components.To_Storage_Array (X, Length);
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Value : Float)
      return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Count;
      X : Float_Integer := To_Float_Integer (Value);
   begin
      if X >= 2 ** (Float'Size - 1) then
         X := Float_Integer'Last - X;
      else
         X := X + 2 ** (Float'Size - 1);
      end if;
      return Float_Integer_Components.To_Storage_Array
        (X, Float'Size / System.Storage_Unit);
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Value : Long_Float)
      return System.Storage_Elements.Storage_Array
   is
      use type System.Storage_Elements.Storage_Count;
      X : Long_Float_Integer := To_Long_Float_Integer (Value);
   begin
      if X >= 2 ** (Long_Float'Size - 1) then
         X := Long_Float_Integer'Last - X;
      else
         X := X + 2 ** (Long_Float'Size - 1);
      end if;
      return Long_Float_Integer_Components.To_Storage_Array
        (X, Long_Float'Size / System.Storage_Unit);
   end To_Storage_Array;

   ----------------------
   -- To_Storage_Array --
   ----------------------

   function To_Storage_Array
     (Value : String;
      Length : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Array
   is
      use System.Storage_Elements;
      Result : Storage_Array (1 .. Length) := (others => 0);
   begin
      pragma Assert (Value'Length < Natural (Length - 2));

      Result (Result'Last - 1) :=
        Storage_Element (Value'Length / (2**System.Storage_Unit));
      Result (Result'Last) :=
        Storage_Element (Value'Length mod (2**System.Storage_Unit));
      for I in Value'Range loop
         declare
            Index : constant Storage_Offset :=
                      Storage_Offset (I - Value'First) +  Result'First;
         begin
            exit when Index not in Result'Range;
            Result (Index) := Character'Pos (Value (I));
         end;
      end loop;
      return Result;
   end To_Storage_Array;

end Marlowe.Key_Storage;
