package body Marlowe.Btree.Images is

   -----------
   -- Image --
   -----------

   function Image (Index : Page_Index) return String is
      Result : String := Index'Img;
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Image;

end Marlowe.Btree.Images;

