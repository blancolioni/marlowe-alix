private with Ada.Finalization;
with Ada.Strings.Unbounded;

with System.Storage_Elements;

package Marlowe.Data_Stores is

   type Root_Data_Store_Cursor
     (Key_Length : System.Storage_Elements.Storage_Count)
     is abstract tagged private;

   subtype Data_Store_Cursor is Root_Data_Store_Cursor'Class;

   procedure Next (Current : in out Root_Data_Store_Cursor)
   is abstract;

   procedure Previous (Current : in out Root_Data_Store_Cursor)
   is abstract;

   function Get_Key (From : Root_Data_Store_Cursor'Class)
                     return System.Storage_Elements.Storage_Array;

   procedure Get_Key (From : Root_Data_Store_Cursor;
                      Key  :    out System.Storage_Elements.Storage_Array)
   is abstract;

   function Valid (Item : Root_Data_Store_Cursor) return Boolean
                   is abstract;

   procedure Release (Item : in out Root_Data_Store_Cursor) is abstract;

   type Data_Store_Interface is interface;

   procedure Create (Store  : in out Data_Store_Interface;
                     Name   : in     String;
                     Magic  : in     Marlowe_Magic_Number)
   is abstract;

   procedure Open (Store  : in out Data_Store_Interface;
                   Name   : in     String;
                   Magic  : in     Marlowe_Magic_Number)
   is abstract;

   procedure Close (Store : in out Data_Store_Interface)
   is abstract;

   function Add_Table
     (Store    : in out Data_Store_Interface;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
      return Table_Index
      is abstract;

   procedure Add_Table
     (Store    : in out Data_Store_Interface'Class;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count);

   function Get_Record_Name
     (Store : Data_Store_Interface;
      Index : in Table_Index)
      return String
      is abstract;

   function Insert_Record
     (Store : in out Data_Store_Interface;
      Table : in Table_Index)
      return Database_Index
      is abstract;
   --  Allocates space in the database for a new record.
   --  Returns the index of the new record, which can
   --  be used by Write_Record to set the data
   --  The record is guaranteed to be zero-filled.

   procedure Get_Record (Store    : in out Data_Store_Interface;
                         Index    : in Table_Index;
                         Db_Index : in Database_Index;
                         Data     : in System.Address)
   is abstract;

   procedure Write_Record (Store    : in out Data_Store_Interface;
                           Index    : in Table_Index;
                           Db_Index : in Database_Index;
                           Data     : in System.Address)
   is abstract;

   procedure Delete_Record (Store    : in out Data_Store_Interface;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index)
   is abstract;

   function Record_Length (Store  : Data_Store_Interface;
                           Table  : in Table_Index)
                           return System.Storage_Elements.Storage_Count
                           is abstract;

   function Valid_Index (Store    : Data_Store_Interface;
                         Table    : in Table_Index;
                         Db_Index : in Database_Index)
                         return Boolean
                         is abstract;

   function Last_Index (Store    : Data_Store_Interface;
                        Table    : in Table_Index)
                        return Database_Index
                        is abstract;

   function Deleted_Record (Store    : Data_Store_Interface;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index)
                            return Boolean
                            is abstract;

   type Key_Reference is
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Key_Index    : Positive;
         Table        : Table_Index;
         Key_Length   : System.Storage_Elements.Storage_Count;
      end record;

   function Get_Reference (Store     : Data_Store_Interface;
                           Name      : in     String)
                           return Key_Reference
                           is abstract;

   function Get_Key_Table_Index (Reference : Key_Reference)
                                 return Table_Index;

   function Get_Key_Length (Reference : Key_Reference)
                            return System.Storage_Elements.Storage_Offset;

   function Add_Key
     (Store        : in out Data_Store_Interface;
      Name         : in    String;
      Record_No    : in    Table_Index;
      Length       : in    System.Storage_Elements.Storage_Offset)
      return Key_Reference
      is abstract;

   procedure Insert (Store      : in out Data_Store_Interface;
                     Reference  : in Key_Reference;
                     Key        : in System.Storage_Elements.Storage_Array)
   is abstract;

   procedure Delete (Store      : in out Data_Store_Interface;
                     Reference  : in Key_Reference;
                     Key        : in System.Storage_Elements.Storage_Array)
   is abstract;

   function Search
     (Store           : Data_Store_Interface;
      Reference       : Key_Reference;
      Start           : System.Storage_Elements.Storage_Array;
      Finish          : System.Storage_Elements.Storage_Array;
      Start_Interval  : Interval_Type;
      Finish_Interval : Interval_Type;
      Scan            : Scan_Type)
      return Data_Store_Cursor
      is abstract;

   function Search
     (Store       : Data_Store_Interface'Class;
      Reference   : Key_Reference;
      Scan        : Scan_Type)
      return Data_Store_Cursor;

   function Exists (Store     : Data_Store_Interface'Class;
                    Reference : Key_Reference;
                    Key       : System.Storage_Elements.Storage_Array)
                    return Boolean;

   function Create_Field_Extension
     (Store     : in out Data_Store_Interface)
      return File_And_Page
      is abstract;

   function Read_Field_Extension
     (Store     : Data_Store_Interface;
      Reference : File_And_Page)
      return System.Storage_Elements.Storage_Array
      is abstract;

   procedure Write_Field_Extension
     (Store : in out Data_Store_Interface;
      Reference : File_And_Page;
      Data      : System.Storage_Elements.Storage_Array)
   is abstract;

   type Data_Store is access all Data_Store_Interface'Class;

private

   type Root_Data_Store_Cursor
     (Key_Length : System.Storage_Elements.Storage_Count)
     is abstract new Ada.Finalization.Controlled with null record;

end Marlowe.Data_Stores;
