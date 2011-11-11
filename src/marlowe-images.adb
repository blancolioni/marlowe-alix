package body Marlowe.Images is

   -------------------------
   -- Storage_Array_Image --
   -------------------------

   function Storage_Array_Image
     (Item : System.Storage_Elements.Storage_Array)
     return String
   is
      use type System.Storage_Elements.Storage_Element;
      use type System.Storage_Elements.Storage_Offset;
      Image  : String (1 .. Item'Length * 2);
      Hex_Digit : constant String := "0123456789ABCDEF";
   begin
      for I in Item'Range loop
         Image (Positive ((I - Item'First) * 2 + 1)) :=
           Hex_Digit (Positive (Item (I) / 16 + 1));
         Image (Positive ((I - Item'First) * 2 + 2)) :=
           Hex_Digit (Positive (Item (I) mod 16 + 1));
      end loop;
      return Image;
   end Storage_Array_Image;

end Marlowe.Images;
