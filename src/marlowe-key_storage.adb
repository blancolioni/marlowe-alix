package body Marlowe.Key_Storage is

   type Unsigned_Integer is mod 2 ** Integer'Size;


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
         for I in reverse Key_Value'Range loop
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
         Result (X) := Hex (Natural (Key (I)) / 16);
         X := X + 1;
         Result (X) := Hex (Natural (Key (I)) mod 16);
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
      Result : Database_Index := 0;
   begin
      for I in reverse Key_Value'Range loop
         Result := Result * 256 + Database_Index (Key_Value (I));
      end loop;
      return Result;
   end To_Database_Index;

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
         Result (Storage_Offset (I - Value'First) +  Result'First) :=
           Character'Pos (Value (I));
      end loop;
      return Result;
   end To_Storage_Array;

end Marlowe.Key_Storage;
