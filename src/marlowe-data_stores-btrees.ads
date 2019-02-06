private with Marlowe.Btree_Handles;

package Marlowe.Data_Stores.Btrees is

   type Btree_Data_Store is
     new Data_Store_Interface with private;

   overriding procedure Create (Store  : in out Btree_Data_Store;
                     Name   : in     String;
                     Magic  : in     Marlowe_Magic_Number);

   overriding procedure Open (Store  : in out Btree_Data_Store;
                   Name   : in     String;
                   Magic  : in     Marlowe_Magic_Number);

   overriding procedure Close (Store : in out Btree_Data_Store);

   overriding function Add_Table
     (Store    : in out Btree_Data_Store;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
      return Table_Index;

   overriding function Get_Record_Name
     (Store : Btree_Data_Store;
      Table : in Table_Index)
      return String;

   overriding function Insert_Record
     (Store : in out Btree_Data_Store;
      Table : in Table_Index)
      return Database_Index;

   overriding procedure Get_Record (Store    : in out Btree_Data_Store;
                         Table    : in Table_Index;
                         Db_Index : in Database_Index;
                         Data     : in System.Address);

   overriding procedure Write_Record (Store    : in out Btree_Data_Store;
                           Table    : in Table_Index;
                           Db_Index : in Database_Index;
                           Data     : in System.Address);

   overriding procedure Delete_Record (Store    : in out Btree_Data_Store;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index);

   overriding function Record_Length (Store  : Btree_Data_Store;
                           Table  : in Table_Index)
                           return System.Storage_Elements.Storage_Count;

   overriding function Valid_Index (Store    : Btree_Data_Store;
                         Table    : in Table_Index;
                         Db_Index : in Database_Index)
                         return Boolean;

   overriding function Last_Index (Store    : Btree_Data_Store;
                        Table    : in Table_Index)
                        return Database_Index;

   overriding function Deleted_Record (Store    : Btree_Data_Store;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index)
                            return Boolean;

   overriding function Get_Reference (Store     : Btree_Data_Store;
                           Name      : in     String)
                           return Key_Reference;

   overriding function Add_Key
     (Store        : in out Btree_Data_Store;
      Name         : in    String;
      Table        : in    Table_Index;
      Length       : in    System.Storage_Elements.Storage_Offset)
      return Key_Reference;

   overriding procedure Insert (Store      : in out Btree_Data_Store;
                     Reference  : in Key_Reference;
                     Key        : in System.Storage_Elements.Storage_Array);

   overriding procedure Delete (Store      : in out Btree_Data_Store;
                     Reference  : in Key_Reference;
                     Key        : in System.Storage_Elements.Storage_Array);

   overriding function Search
     (Store           : Btree_Data_Store;
      Reference       : Key_Reference;
      Start           : System.Storage_Elements.Storage_Array;
      Finish          : System.Storage_Elements.Storage_Array;
      Start_Interval  : Interval_Type;
      Finish_Interval : Interval_Type;
      Scan            : Scan_Type)
      return Data_Store_Cursor;

   overriding function Create_Field_Extension
     (Store     : in out Btree_Data_Store)
      return File_And_Page;

   overriding function Read_Field_Extension
     (Store     : Btree_Data_Store;
      Reference : File_And_Page)
      return System.Storage_Elements.Storage_Array;

   overriding procedure Write_Field_Extension
     (Store : in out Btree_Data_Store;
      Reference : File_And_Page;
      Data      : System.Storage_Elements.Storage_Array);

   overriding function Get_Data_Store_Information
     (Store : Btree_Data_Store)
      return Marlowe.Database.Database_Information;

private

   type Btree_Data_Store is
     new Data_Store_Interface with
      record
         Btree : Marlowe.Btree_Handles.Btree_Handle;
      end record;

end Marlowe.Data_Stores.Btrees;
