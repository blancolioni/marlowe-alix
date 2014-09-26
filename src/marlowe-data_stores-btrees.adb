package body Marlowe.Data_Stores.Btrees is

   use Marlowe.Btree_Handles;

   type Btree_Data_Store_Cursor
     (Key_Length : System.Storage_Elements.Storage_Count)
   is
     new Root_Data_Store_Cursor (Key_Length) with
      record
         Mark : Btree_Mark (Key_Length);
      end record;

   overriding procedure Next (Current : in out Btree_Data_Store_Cursor);
   overriding procedure Previous (Current : in out Btree_Data_Store_Cursor);
   overriding procedure Get_Key (From : Btree_Data_Store_Cursor;
                      Key  :    out System.Storage_Elements.Storage_Array);
   overriding function Valid (Item : Btree_Data_Store_Cursor) return Boolean;
   overriding procedure Release (Item : in out Btree_Data_Store_Cursor);

   -------------
   -- Add_Key --
   -------------

   overriding function Add_Key
     (Store        : in out Btree_Data_Store;
      Name         : in    String;
      Table        : in    Table_Index;
      Length       : in    System.Storage_Elements.Storage_Offset)
      return Key_Reference
   is
   begin
      return Add_Key (Store.Btree, Name, Table, Length);
   end Add_Key;

   ---------------
   -- Add_Table --
   ---------------

   overriding function Add_Table
     (Store    : in out Btree_Data_Store;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
      return Table_Index
   is
   begin
      return Add_Table (Store.Btree, Name, Length);
   end Add_Table;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Store : in out Btree_Data_Store) is
   begin
      Close (Store.Btree);
   end Close;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Store  : in out Btree_Data_Store;
      Name   : in     String;
      Magic  : in     Marlowe_Magic_Number)
   is
   begin
      Create (Store.Btree, Name, Magic);
   end Create;

   ----------------------------
   -- Create_Field_Extension --
   ----------------------------

   overriding function Create_Field_Extension
     (Store     : in out Btree_Data_Store)
      return File_And_Page
   is
   begin
      return Create_Field_Extension (Store.Btree);
   end Create_Field_Extension;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Store      : in out Btree_Data_Store;
      Reference  : in Key_Reference;
      Key        : in System.Storage_Elements.Storage_Array)
   is
   begin
      Delete (Store.Btree, Reference, Key);
   end Delete;

   -------------------
   -- Delete_Record --
   -------------------

   overriding procedure Delete_Record
     (Store    : in out Btree_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index)
   is
   begin
      Delete_Record (Store.Btree, Table, Db_Index);
   end Delete_Record;

   --------------------
   -- Deleted_Record --
   --------------------

   overriding function Deleted_Record
     (Store    : Btree_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index)
      return Boolean
   is
   begin
      return Deleted_Record (Store.Btree, Table, Db_Index);
   end Deleted_Record;

   -------------
   -- Get_Key --
   -------------

   overriding procedure Get_Key
     (From : Btree_Data_Store_Cursor;
      Key  :    out System.Storage_Elements.Storage_Array)
   is
   begin
      Get_Key (From.Mark, Key);
   end Get_Key;

   ----------------
   -- Get_Record --
   ----------------

   overriding procedure Get_Record
     (Store    : in out Btree_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index;
      Data     : in System.Address)
   is
   begin
      Get_Record (Store.Btree, Table, Db_Index, Data);
   end Get_Record;

   ---------------------
   -- Get_Record_Name --
   ---------------------

   overriding function Get_Record_Name
     (Store : Btree_Data_Store;
      Table : in Table_Index)
      return String
   is
   begin
      return Get_Record_Name (Store.Btree, Table);
   end Get_Record_Name;

   -------------------
   -- Get_Reference --
   -------------------

   overriding function Get_Reference
     (Store     : Btree_Data_Store;
      Name      : in     String)
      return Key_Reference
   is
   begin
      return Get_Reference (Store.Btree, Name);
   end Get_Reference;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Store      : in out Btree_Data_Store;
      Reference  : in Key_Reference;
      Key        : in System.Storage_Elements.Storage_Array)
   is
   begin
      Insert (Store.Btree, Reference, Key);
   end Insert;

   -------------------
   -- Insert_Record --
   -------------------

   overriding function Insert_Record
     (Store : in out Btree_Data_Store;
      Table : in Table_Index)
      return Database_Index
   is
   begin
      return Insert_Record (Store.Btree, Table);
   end Insert_Record;

   ----------------
   -- Last_Index --
   ----------------

   overriding function Last_Index
     (Store    : Btree_Data_Store;
      Table    : in Table_Index)
      return Database_Index
   is
   begin
      return Last_Index (Store.Btree, Table);
   end Last_Index;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Current : in out Btree_Data_Store_Cursor)
   is
   begin
      Next (Current.Mark);
   end Next;

   ----------
   -- Open --
   ----------

   overriding procedure Open
     (Store  : in out Btree_Data_Store;
      Name   : in     String;
      Magic  : in     Marlowe_Magic_Number)
   is
   begin
      Open (Store.Btree, Name, Magic);
   end Open;

   --------------
   -- Previous --
   --------------

   overriding procedure Previous
     (Current : in out Btree_Data_Store_Cursor)
   is
   begin
      Next (Current.Mark);
   end Previous;

   --------------------------
   -- Read_Field_Extension --
   --------------------------

   overriding function Read_Field_Extension
     (Store     : Btree_Data_Store;
      Reference : File_And_Page)
      return System.Storage_Elements.Storage_Array
   is
   begin
      return Read_Field_Extension (Store.Btree, Reference);
   end Read_Field_Extension;

   -------------------
   -- Record_Length --
   -------------------

   overriding function Record_Length
     (Store  : Btree_Data_Store;
      Table  : in Table_Index)
      return System.Storage_Elements.Storage_Count
   is
   begin
      return Record_Length (Store.Btree, Table);
   end Record_Length;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Btree_Data_Store_Cursor) is
   begin
      Release (Item.Mark);
   end Release;

   ------------
   -- Search --
   ------------

   overriding function Search
     (Store           : Btree_Data_Store;
      Reference       : Key_Reference;
      Start           : System.Storage_Elements.Storage_Array;
      Finish          : System.Storage_Elements.Storage_Array;
      Start_Interval  : Interval_Type;
      Finish_Interval : Interval_Type;
      Scan            : Scan_Type)
      return Data_Store_Cursor
   is
   begin
      return Result : Btree_Data_Store_Cursor (Start'Length) do
         declare
            Mark : constant Btree_Mark :=
                     Search (Store.Btree, Reference, Start, Finish,
                             Start_Interval, Finish_Interval, Scan);
         begin
            Result.Mark := Mark;
         end;
      end return;
   end Search;

   -----------
   -- Valid --
   -----------

   overriding function Valid
     (Item : Btree_Data_Store_Cursor)
      return Boolean
   is
   begin
      return Valid (Item.Mark);
   end Valid;

   -----------------
   -- Valid_Index --
   -----------------

   overriding function Valid_Index
     (Store    : Btree_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index)
      return Boolean
   is
   begin
      return Valid_Index (Store.Btree, Table, Db_Index);
   end Valid_Index;

   ---------------------------
   -- Write_Field_Extension --
   ---------------------------

   overriding procedure Write_Field_Extension
     (Store : in out Btree_Data_Store;
      Reference : File_And_Page;
      Data      : System.Storage_Elements.Storage_Array)
   is
   begin
      Write_Field_Extension (Store.Btree, Reference, Data);
   end Write_Field_Extension;

   ------------------
   -- Write_Record --
   ------------------

   overriding procedure Write_Record
     (Store    : in out Btree_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index;
      Data     : in System.Address)
   is
   begin
      Write_Record (Store.Btree, Table, Db_Index, Data);
   end Write_Record;

end Marlowe.Data_Stores.Btrees;
