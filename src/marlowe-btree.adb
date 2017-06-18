with Marlowe.Pages;
with Marlowe.Tables;

package body Marlowe.Btree is

   -----------
   -- Image --
   -----------

   function Image (P : Page_Index) return String is
   begin
      return Page_Index'Image (P);
   end Image;

   ----------------------
   -- Records_Per_Page --
   ----------------------

   function Records_Per_Page
     (Record_Length : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Record_Length);
      --  It's there in case we decide to perform the
      --  small optimisation for small records
      --  (we can save 8 bytes per record if length <= 64)
   begin
      return Marlowe.Pages.Elements_Per_Page
        (Marlowe.Tables.Max_Direct_Record_Storage_Units);
   end Records_Per_Page;

end Marlowe.Btree;
