with Marlowe.Btree.Disk_Pages;

generic
   type Key_Type is private;
   Min_Keys_In_Page : Positive;
   Unique           : Boolean;
   with function "=" (Left, Right : Key_Type) return Boolean;
   with function "<" (Left, Right : Key_Type) return Boolean;
   with function Image (X : Key_Type) return String;
   with package Disk_Pages is
     new Marlowe.Btree.Disk_Pages (Key_Type, Min_Keys_In_Page, Unique,
                                   "=", "<", Image);
package Marlowe.Btree.Files is

   procedure Read_Page (Index   : in     Page_Index;
                        Page    :    out Disk_Pages.Btree_Disk_Page);

   procedure Write_Page (Page   : in     Disk_Pages.Btree_Disk_Page);

   procedure New_Page (Page : out Disk_Pages.Btree_Disk_Page);

   procedure Create (Name : in     String);

   procedure Open (Name : in     String);

   procedure Close;

   function Root_Page return Page_Index;
   procedure Set_Root (Page  : in Page_Index);

end Marlowe.Btree.Files;

