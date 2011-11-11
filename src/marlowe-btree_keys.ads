with System.Storage_Elements;

package Marlowe.Btree_Keys is

   type Btree_Key_Type is (Signed_Integer_Key,
                           Unsigned_Integer_Key,
                           String_Key,
                           Raw_Key);

   type Key_Component is private;
   function Make_Component (Key_Type : Btree_Key_Type;
                            Length   : Natural)
                           return Key_Component;

   function Get_Key_Type (From : Key_Component) return Btree_Key_Type;
   function Get_Length   (From : Key_Component) return Natural;

   type Component_Array is array (Positive range <>) of Key_Component;

   type Compare_Result is (Less, Equal, Greater);

   function Compare (Using_Key   : Component_Array;
                     Left, Right : System.Storage_Elements.Storage_Array)
                    return Compare_Result;

   function Equal (Using_Key   : Component_Array;
                   Left, Right : System.Storage_Elements.Storage_Array)
                  return Boolean;

   function To_Btree_Key (Component : Key_Component;
                          Item      : Integer)
                         return System.Storage_Elements.Storage_Array;

   function To_Btree_Key (Item      : Database_Index)
                         return System.Storage_Elements.Storage_Array;

   function To_Database_Index (Key : System.Storage_Elements.Storage_Array)
                               return Database_Index;

   function To_Integer
     (Component : Key_Component;
      Item      : System.Storage_Elements.Storage_Array)
     return Integer;

   generic
      type Component_Type is mod <>;
   package Unsigned_Key_Components is
      function To_Btree_Key
        (Component : Key_Component;
         Item      : Component_Type)
        return System.Storage_Elements.Storage_Array;
      function To_Ada_Type
        (Component : Key_Component;
         Item      : System.Storage_Elements.Storage_Array)
        return Component_Type;
   end Unsigned_Key_Components;

   generic
      type Component_Type is range <>;
   package Signed_Key_Components is
      function To_Btree_Key
        (Component : Key_Component;
         Item      : Component_Type)
        return System.Storage_Elements.Storage_Array;
      function To_Ada_Type
        (Component : Key_Component;
         Item      : System.Storage_Elements.Storage_Array)
        return Component_Type;
   end Signed_Key_Components;

   generic
      type Component_Type is (<>);
   package Discrete_Key_Components is
      function To_Btree_Key
        (Component : Key_Component;
         Item      : Component_Type)
        return System.Storage_Elements.Storage_Array;
      function To_Ada_Type
        (Component : Key_Component;
         Item      : System.Storage_Elements.Storage_Array)
        return Component_Type;
   end Discrete_Key_Components;

   function To_Btree_Key (Component : Key_Component;
                          Key       : String)
                         return System.Storage_Elements.Storage_Array;

   function To_String (Component : Key_Component;
                       Item      : System.Storage_Elements.Storage_Array)
                      return String;

   function Image (Item : System.Storage_Elements.Storage_Array)
                  return String;

private

   pragma Inline (Compare);

   type Key_Component_Length is range 0 .. 4095;
   for Key_Component_Length'Size use 12;

   type Key_Component is
      record
         Length   : Key_Component_Length;
         Reversed : Boolean;
         Key_Type : Btree_Key_Type;
      end record;

   for Key_Component use
      record
         Length at 0 range 0 .. 11;
         Reversed at 0 range 12 .. 12;
         Key_Type at 0 range 14 .. 15;
      end record;
   for Key_Component'Size use 16;

end Marlowe.Btree_Keys;
