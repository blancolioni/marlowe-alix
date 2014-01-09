private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;

package Marlowe.Data_Stores.Memory is

   type Memory_Data_Store is
     new Data_Store_Interface with private;

   overriding procedure Create (Store  : in out Memory_Data_Store;
                     Name   : in     String;
                     Magic  : in     Marlowe_Magic_Number);

   overriding procedure Open (Store  : in out Memory_Data_Store;
                   Name   : in     String;
                   Magic  : in     Marlowe_Magic_Number);

   overriding procedure Close (Store : in out Memory_Data_Store);

   overriding function Add_Table
     (Store    : in out Memory_Data_Store;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
      return Table_Index;

   overriding function Get_Record_Name
     (Store : Memory_Data_Store;
      Table : in Table_Index)
      return String;

   overriding function Insert_Record
     (Store : in out Memory_Data_Store;
      Table : in Table_Index)
      return Database_Index;

   overriding procedure Get_Record (Store    : in out Memory_Data_Store;
                         Table    : in Table_Index;
                         Db_Index : in Database_Index;
                         Data     : in System.Address);

   overriding procedure Write_Record (Store    : in out Memory_Data_Store;
                           Table    : in Table_Index;
                           Db_Index : in Database_Index;
                           Data     : in System.Address);

   overriding procedure Delete_Record (Store    : in out Memory_Data_Store;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index);

   overriding function Record_Length (Store  : Memory_Data_Store;
                           Table  : in Table_Index)
                           return System.Storage_Elements.Storage_Count;

   overriding function Valid_Index (Store    : Memory_Data_Store;
                         Table    : in Table_Index;
                         Db_Index : in Database_Index)
                         return Boolean;

   overriding function Last_Index (Store    : Memory_Data_Store;
                        Table    : in Table_Index)
                        return Database_Index;

   overriding function Deleted_Record (Store    : Memory_Data_Store;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index)
                            return Boolean;

   overriding function Get_Reference (Store     : Memory_Data_Store;
                           Name      : in     String)
                           return Key_Reference;

   overriding function Add_Key
     (Store        : in out Memory_Data_Store;
      Name         : in    String;
      Table        : in    Table_Index;
      Length       : in    System.Storage_Elements.Storage_Offset)
      return Key_Reference;

   overriding procedure Insert (Store      : in out Memory_Data_Store;
                     Reference  : in Key_Reference;
                     Key        : in System.Storage_Elements.Storage_Array);

   overriding procedure Delete (Store      : in out Memory_Data_Store;
                     Reference  : in Key_Reference;
                     Key        : in System.Storage_Elements.Storage_Array);

   overriding function Search
     (Store           : Memory_Data_Store;
      Reference       : Key_Reference;
      Start           : System.Storage_Elements.Storage_Array;
      Finish          : System.Storage_Elements.Storage_Array;
      Start_Interval  : Interval_Type;
      Finish_Interval : Interval_Type;
      Scan            : Scan_Type)
      return Data_Store_Cursor;

   overriding function Create_Field_Extension
     (Store     : in out Memory_Data_Store)
      return File_And_Page;

   overriding function Read_Field_Extension
     (Store     : Memory_Data_Store;
      Reference : File_And_Page)
      return System.Storage_Elements.Storage_Array;

   overriding procedure Write_Field_Extension
     (Store : in out Memory_Data_Store;
      Reference : File_And_Page;
      Data      : System.Storage_Elements.Storage_Array);

private

   package Key_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => System.Storage_Elements.Storage_Array,
        Element_Type => Boolean,
        "<"          => System.Storage_Elements."<");

   type Key_Info is
      record
         Reference : Key_Reference;
         Map       : Key_Maps.Map;
      end record;

   package Record_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Table_Info is
      record
         Name    : Ada.Strings.Unbounded.Unbounded_String;
         Length  : System.Storage_Elements.Storage_Count;
         Records : Record_Vectors.Vector;
      end record;

   package Table_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Table_Info);

   package Key_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Key_Info);

   package Field_Extension_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, System.Storage_Elements.Storage_Array,
        System.Storage_Elements."=");

   type Memory_Data_Store is
     new Data_Store_Interface with
      record
         Magic      : Marlowe_Magic_Number;
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Tables     : Table_Info_Vectors.Vector;
         Keys       : Key_Info_Vectors.Vector;
         Extensions : Field_Extension_Vectors.Vector;
      end record;

end Marlowe.Data_Stores.Memory;
