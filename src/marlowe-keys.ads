with Marlowe.Btree.Disk_Pages;
with Marlowe.Btree.Pages;

generic
   type Key_Type is private;
   Minimum_Degree : Marlowe.Btree.Slot;
   --   with function Image (Item : Key_Type) return String;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
package Marlowe.Keys is

   function Exists (Key     : Key_Type) return Boolean;

   procedure Insert (Key     : in     Key_Type);

   procedure Delete (Key     : Key_Type);

   type Mark is private;

   function Empty_Mark return Mark;

   function Search (Start           : Key_Type;
                    Finish          : Key_Type;
                    Start_Interval  : Interval_Type;
                    Finish_Interval : Interval_Type;
                    Scan            : Scan_Type)
                   return Mark;

   function Search (Scan      : Scan_Type)
                   return Mark;

   procedure Next (Current : in out Mark);

   function Valid (Current : Mark) return Boolean;
   function Get_Key (Current : Mark) return Key_Type;

   procedure Open (File_Path  : in String;
                   Cache_Size : in Natural := 8192);

   procedure Create (File_Path   : in String;
                     Cache_Size  : in Natural := 8192);
   procedure Close;

   function Tree_OK return Boolean;

   procedure Dump_Btree;

private

   package Pages is
      new Marlowe.Btree.Pages (Key_Type, Minimum_Degree, "=", "<");

   type Mark (Use_Key : Boolean := True) is
      record
         Page        : Pages.Btree_Page;
         Slot        : Pages.Key_Index;
         Scan        : Scan_Type;
         case Use_Key is
            when True =>
               Start_Key, Finish_Key           : Key_Type;
               Start_Interval, Finish_Interval : Interval_Type;
            when False =>
               null;
         end case;
      end record;

end Marlowe.Keys;
