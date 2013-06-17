package Marlowe.Page_Types is

   type Page_Type is
     (No_Page_Type,
      File_Header_Page_Type,
      Free_Map_Page_Type,
      Btree_Header_Page_Type, Btree_Node_Page_Type, Btree_Leaf_Page_Type,
      Data_Definition_Page_Type, Page_Pointer_Page_Type,
      Direct_Data_Pointer_Page_Type, Data_Page_Type, Data_Overflow_Page_Type,
      Log_Page_Type, Field_Extension_Page_Type);

end Marlowe.Page_Types;

