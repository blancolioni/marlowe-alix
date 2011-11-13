with System.Storage_Elements;

package Marlowe.Key_Storage is

   function To_Storage_Array (Value : Database_Index)
                              return System.Storage_Elements.Storage_Array;

   function To_Storage_Array
     (Value : Integer;
      Length : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Array;

   function To_Storage_Array
     (Value : Float)
      return System.Storage_Elements.Storage_Array;

   function To_Storage_Array
     (Value : Long_Float)
      return System.Storage_Elements.Storage_Array;

   function To_Storage_Array
     (Value : String;
      Length : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Array;

   function To_Database_Index
     (Key_Value : System.Storage_Elements.Storage_Array)
      return Database_Index;

   type Compare_Result is (Less, Equal, Greater);

   function Compare (Left, Right : System.Storage_Elements.Storage_Array)
                    return Compare_Result;

   function Equal (Left, Right : System.Storage_Elements.Storage_Array)
                   return Boolean;

   function Image (Key : System.Storage_Elements.Storage_Array)
                   return String;

   generic
      type Component_Type is mod <>;
   package Unsigned_Key_Components is
      function To_Storage_Array
        (Value  : Component_Type;
         Length : System.Storage_Elements.Storage_Count)
         return System.Storage_Elements.Storage_Array;

      function To_Ada_Type
        (Key_Value : System.Storage_Elements.Storage_Array)
        return Component_Type;
   end Unsigned_Key_Components;

end Marlowe.Key_Storage;
