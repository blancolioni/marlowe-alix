with Ada.Finalization;

with System.Storage_Elements;

--  with Marlowe.Btree_Keys;
with Marlowe.Btree_Page_Handles;

package Marlowe.Btree_Handles is

   type Btree_Handle is private;

   type Btree_Reference is private;

   Bad_Magic_Number : exception;

   type Btree_Mark (Key_Length : System.Storage_Elements.Storage_Count) is
     private;

   procedure Create (Handle : in out Btree_Handle;
                     Name   : in     String;
                     Magic  : in     Btree_Magic);

   procedure Open (Handle : in out Btree_Handle;
                   Name   : in     String;
                   Magic  : in     Btree_Magic);

   procedure Close (Handle : in out Btree_Handle);

   function Add_Table
     (Handle   : in Btree_Handle;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
      return Table_Index;

   procedure Add_Table
     (Handle   : in Btree_Handle;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count);

   function Get_Record_Name (Handle    : in Btree_Handle;
                             Index     : in Table_Index)
                             return String;

--     function Get_Record_Length (Handle   : in Btree_Handle;
--                                 Index    : in Table_Index)
--                                 return a;

   function Insert_Record
     (Handle    : in Btree_Handle;
      Table     : in Table_Index)
     return Database_Index;
   --  Allocates space in the database for a new record.
   --  Returns the index of the new record, which can
   --  be used by Write_Record to set the data

   procedure Get_Record (Handle   : in Btree_Handle;
                         Index    : in Table_Index;
                         Db_Index : in Database_Index;
                         Data     : in System.Address);

   procedure Write_Record (Handle   : in Btree_Handle;
                           Index    : in Table_Index;
                           Db_Index : in Database_Index;
                           Data     : in System.Address);

   procedure Delete_Record (Handle   : in Btree_Handle;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index);

   function Valid_Index (Handle   : in Btree_Handle;
                         Table    : in Table_Index;
                         Db_Index : in Database_Index)
                        return Boolean;

   function Deleted_Record (Handle   : in Btree_Handle;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index)
                           return Boolean;

   function Get_Reference (Handle    : in     Btree_Handle;
                           Name      : in     String)
                          return Btree_Reference;

   function Get_Key_Table_Index (Reference : Btree_Reference)
                                 return Table_Index;

   function Get_Key_Length (Reference : Btree_Reference)
                           return System.Storage_Elements.Storage_Offset;

   function Add_Key
     (Handle       : in    Btree_Handle;
      Name         : in    String;
      Record_No    : in    Table_Index;
      Length       : in    System.Storage_Elements.Storage_Offset)
      return Btree_Reference;

   procedure Insert (Handle     : in Btree_Handle;
                     Reference  : in Btree_Reference;
                     Key        : in System.Storage_Elements.Storage_Array);

   procedure Delete (Handle     : in Btree_Handle;
                     Reference  : in Btree_Reference;
                     Key        : in System.Storage_Elements.Storage_Array);

   function Search
     (Handle          : Btree_Handle;
      Reference       : Btree_Reference;
      Start           : System.Storage_Elements.Storage_Array;
      Finish          : System.Storage_Elements.Storage_Array;
      Start_Interval  : Interval_Type;
      Finish_Interval : Interval_Type;
      Scan            : Scan_Type)
     return Btree_Mark;

   function Search
     (Handle          : Btree_Handle;
      Reference       : Btree_Reference;
      Scan            : Scan_Type)
     return Btree_Mark;

   function Exists (Handle    : Btree_Handle;
                    Reference : Btree_Reference;
                    Key       : System.Storage_Elements.Storage_Array)
                   return Boolean;

   procedure Next (Current : in out Btree_Mark);

   function Get_Key (From : Btree_Mark)
                    return System.Storage_Elements.Storage_Array;

   procedure Get_Key (From : in     Btree_Mark;
                      Key  :    out System.Storage_Elements.Storage_Array);

   function Valid (Item : Btree_Mark) return Boolean;

   procedure Release (Mark : in out Btree_Mark);
   --  Normally a btree mark keeps a shared lock on its page until
   --  the mark is finalised.  However, it's possible to release it
   --  early using this procedure.  The mark can no longer be used
   --  after it is released.


   function Check_Tree (Handle    : Btree_Handle;
                        Reference : Btree_Reference)
                       return Boolean;

   procedure Dump_Tree (Handle    : Btree_Handle;
                        Reference : Btree_Reference;
                        To_Output : Boolean          := False);

   procedure Get_Cache_Statistics (Handle : in     Btree_Handle;
                                   Blocks :    out Natural;
                                   Pages  :    out Natural;
                                   Hits   :    out Natural;
                                   Misses :    out Natural);

private

   type Btree_Handle_Record;

   type Btree_Handle is access Btree_Handle_Record;

   type Btree_Reference_Record;

   type Btree_Reference is access Btree_Reference_Record;

   use System.Storage_Elements;

   type Btree_Mark (Key_Length : System.Storage_Elements.Storage_Count) is
     new Ada.Finalization.Controlled with
      record
         Reference       : Btree_Reference;
         Slot            : Slot_Index  := 0;
         Page            : Marlowe.Btree_Page_Handles.Btree_Page_Handle;
         Scan            : Scan_Type;
         Start_Key       : Storage_Array (1 .. Key_Length);
         Finish_Key      : Storage_Array (1 .. Key_Length);
         Start_Interval  : Interval_Type;
         Finish_Interval : Interval_Type;
      end record;

   procedure Adjust (Mark : in out Btree_Mark);
   procedure Finalize (Mark : in out Btree_Mark);

   function Get_Root (Handle    : in Btree_Handle;
                      Reference : in Btree_Reference)
                     return Marlowe.Btree_Page_Handles.Btree_Page_Handle;

end Marlowe.Btree_Handles;
