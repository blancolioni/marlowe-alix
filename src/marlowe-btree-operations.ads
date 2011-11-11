with Marlowe.Btree.Disk_Pages;
with Marlowe.Btree.Pages;

generic
   type Key_Type is private;
   Min_Keys_In_Page : Positive;
   Unique           : Boolean;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with package Disk_Pages is
     new Marlowe.Btree.Disk_Pages (Key_Type, Min_Keys_In_Page, Unique,
                                   "=", "<");
   with package Pages is
     new Marlowe.Btree.Pages (Key_Type, Min_Keys_In_Page, Unique,
                              "=", "<",
                              Disk_Pages => Disk_Pages);

package Marlowe.Btree.Operations is

   function Search (Key     : Key_Type)
                   return Marlowe.Database_Index;


   procedure Insert (Key     : in     Key_Type;
                     Element : in     Marlowe.Database_Index);

   type Mark is private;

   function Empty_Mark return Mark;

   function Search (Key       : Key_Type;
                    Scan      : Scan_Type;
                    Interval  : Interval_Type)
                   return Mark;

   function Search (Scan      : Scan_Type)
                   return Mark;

   procedure Next (Current : in out Mark);

   function Get_With_Element (Key     : Key_Type;
                              Element : Marlowe.Database_Index)
                             return Mark;

   function Valid (Current : Mark) return Boolean;
   function Get_Element (Current : Mark) return Database_Index;

   procedure Delete (Key     : Key_Type;
                     Element : Marlowe.Database_Index);

   procedure Check_Tree;

private

   type Mark (Use_Key : Boolean := True) is
      record
         Page     : Pages.Btree_Page;
         Slot     : Pages.Key_Index;
         Scan     : Scan_Type;
         Interval : Interval_Type;
         Element  : Database_Index;
         case Use_Key is
            when True =>
               Key           : Key_Type;
            when False =>
               null;
         end case;
      end record;

end Marlowe.Btree.Operations;
