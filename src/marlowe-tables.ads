with System.Storage_Elements;

package Marlowe.Tables is

   Max_Direct_Record_Storage_Units : constant := 64;
   Direct_Record_Storage_Units     : constant := 56;

   type Record_Header is
      record
         Overflow : File_Page_And_Slot;
         Data     : System.Storage_Elements.Storage_Array (1 .. 56);
      end record;

   for Record_Header'Size use
     Max_Direct_Record_Storage_Units * System.Storage_Unit;

end Marlowe.Tables;
