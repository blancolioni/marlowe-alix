with System.Storage_Elements;

package Marlowe.Btree is

   type Page_Index is private;
   Null_Page_Index : constant Page_Index;

   type Slot is range 0 .. 65535;

   function Image (P : Page_Index) return String;

   function Records_Per_Page
     (Record_Length : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count;

private

   type Page_Index is new Natural;
   Null_Page_Index : constant Page_Index := 0;

end Marlowe.Btree;
