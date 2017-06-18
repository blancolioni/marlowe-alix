package Marlowe.Page_Types is

   No_Page_Type                  : constant Page_Type := 0;
   File_Header_Page_Type         : constant Page_Type := 1;
   Free_Map_Page_Type            : constant Page_Type := 2;
   Log_Page_Type                 : constant Page_Type := 3;
   Btree_Header_Page_Type        : constant Page_Type := 4;
   Btree_Node_Page_Type          : constant Page_Type := 5;
   Btree_Leaf_Page_Type          : constant Page_Type := 6;
   Data_Definition_Page_Type     : constant Page_Type := 7;
   Page_Pointer_Page_Type        : constant Page_Type := 8;
   Direct_Data_Pointer_Page_Type : constant Page_Type := 9;
   Data_Page_Type                : constant Page_Type := 10;
   Data_Overflow_Page_Type       : constant Page_Type := 11;
   Field_Extension_Page_Type     : constant Page_Type := 12;
   Storage_Element_Page_Type     : constant Page_Type := 13;

   subtype Internal_Page_Type is Page_Type range 1 .. 15;

   First_Available_Page_Type : constant Page_Type :=
                                 Internal_Page_Type'Last + 1;

   type No_Header is null record;

end Marlowe.Page_Types;
