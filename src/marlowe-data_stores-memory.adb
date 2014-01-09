with Ada.Streams.Stream_IO;

with Marlowe.Exceptions;

package body Marlowe.Data_Stores.Memory is

   type Memory_Data_Store_Cursor
     (Key_Length : System.Storage_Elements.Storage_Count)
   is
     new Root_Data_Store_Cursor (Key_Length) with
      record
         Start, Finish : System.Storage_Elements.Storage_Array
           (1 .. Key_Length);
         Start_Interval : Interval_Type;
         Finish_Interval : Interval_Type;
         Current_Position : Key_Maps.Cursor;
      end record;

   overriding procedure Next (Current : in out Memory_Data_Store_Cursor);
   overriding procedure Previous (Current : in out Memory_Data_Store_Cursor);
   overriding procedure Get_Key (From : Memory_Data_Store_Cursor;
                      Key  :    out System.Storage_Elements.Storage_Array);
   overriding function Valid (Item : Memory_Data_Store_Cursor) return Boolean;
   overriding procedure Release (Item : in out Memory_Data_Store_Cursor);

   -------------
   -- Add_Key --
   -------------

   overriding function Add_Key
     (Store        : in out Memory_Data_Store;
      Name         : in    String;
      Table        : in    Table_Index;
      Length       : in    System.Storage_Elements.Storage_Offset)
      return Key_Reference
   is
      Reference : constant Key_Reference :=
                    (Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
                     Key_Index => Store.Keys.Last_Index + 1,
                     Table     => Table,
                     Key_Length    => Length);
      New_Key   : Key_Info;
   begin
      New_Key.Reference := Reference;
      Store.Keys.Append (New_Key);
      return Reference;
   end Add_Key;

   ---------------
   -- Add_Table --
   ---------------

   overriding function Add_Table
     (Store    : in out Memory_Data_Store;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
      return Table_Index
   is
      Info : Table_Info;
   begin
      Info.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Info.Length := Length;
      Store.Tables.Append (Info);
      return Table_Index (Store.Tables.Last_Index);
   end Add_Table;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Store : in out Memory_Data_Store) is
      use Ada.Streams.Stream_IO;
      use Ada.Strings.Unbounded;
      File : File_Type;
   begin
      Create (File, Out_File, To_String (Store.Name));
      Memory_Data_Store'Write (Stream (File), Store);
      Close (File);
   end Close;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Store  : in out Memory_Data_Store;
      Name   : in     String;
      Magic  : in     Marlowe_Magic_Number)
   is
   begin
      Store.Magic := Magic;
      Store.Name  := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Create;

   ----------------------------
   -- Create_Field_Extension --
   ----------------------------

   overriding function Create_Field_Extension
     (Store     : in out Memory_Data_Store)
      return File_And_Page
   is
      Value : System.Storage_Elements.Storage_Array (1 .. 0);
   begin
      Store.Extensions.Append (Value);
      return File_And_Page (Store.Extensions.Last_Index);
   end Create_Field_Extension;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Store      : in out Memory_Data_Store;
      Reference  : in Key_Reference;
      Key        : in System.Storage_Elements.Storage_Array)
   is
      Info : Key_Info renames
               Store.Keys (Reference.Key_Index);
   begin
      Info.Map.Delete (Key);
   end Delete;

   -------------------
   -- Delete_Record --
   -------------------

   overriding procedure Delete_Record
     (Store    : in out Memory_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index)
   is
      use System.Storage_Elements;
      Info : Table_Info renames Store.Tables (Positive (Table));
      Rs : Record_Vectors.Vector renames Info.Records;
      Rec  : constant Storage_Array (1 .. Info.Length) := (others => 0);
   begin
      Rs (Positive (Db_Index)) := Rec;
   end Delete_Record;

   --------------------
   -- Deleted_Record --
   --------------------

   overriding function Deleted_Record
     (Store    : Memory_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index)
      return Boolean
   is
      use System.Storage_Elements;
      Info : Table_Info renames Store.Tables (Positive (Table));
      Rs : Record_Vectors.Vector renames Info.Records;
      Rec  : constant Storage_Array (1 .. Info.Length) := (others => 0);
   begin
      return Rs (Positive (Db_Index)) = Rec;
   end Deleted_Record;

   -------------
   -- Get_Key --
   -------------

   overriding procedure Get_Key (From : Memory_Data_Store_Cursor;
                      Key  :    out System.Storage_Elements.Storage_Array)
   is
   begin
      Key := Key_Maps.Key (From.Current_Position);
   end Get_Key;

   ----------------
   -- Get_Record --
   ----------------

   overriding procedure Get_Record
     (Store    : in out Memory_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index;
      Data     : in System.Address)
   is
      use System.Storage_Elements;
      Info : Table_Info renames Store.Tables (Positive (Table));
      Rs : Record_Vectors.Vector renames Info.Records;
      Rec  : Storage_Array (1 .. Info.Length);
      for Rec'Address use Data;
   begin
      Rec := Rs (Positive (Db_Index));
   end Get_Record;

   ---------------------
   -- Get_Record_Name --
   ---------------------

   overriding function Get_Record_Name
     (Store : Memory_Data_Store;
      Table : in Table_Index)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Store.Tables.Element (Positive (Table)).Name);
   end Get_Record_Name;

   -------------------
   -- Get_Reference --
   -------------------

   overriding function Get_Reference
     (Store     : Memory_Data_Store;
      Name      : in     String)
      return Key_Reference
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Key of Store.Keys loop
         if Key.Reference.Name = Name then
            return Key.Reference;
         end if;
      end loop;
      raise Marlowe.Exceptions.Key_Error with
        "no such key: " & Name;
   end Get_Reference;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Store      : in out Memory_Data_Store;
      Reference  : in Key_Reference;
      Key        : in System.Storage_Elements.Storage_Array)
   is
      Info : Key_Info renames
               Store.Keys (Reference.Key_Index);
   begin
      Info.Map.Insert (Key, True);
   end Insert;

   -------------------
   -- Insert_Record --
   -------------------

   overriding function Insert_Record
     (Store : in out Memory_Data_Store;
      Table : in Table_Index)
      return Database_Index
   is
      use System.Storage_Elements;
      Info : Table_Info renames Store.Tables (Positive (Table));
      Rs : Record_Vectors.Vector renames Info.Records;
      Empty : constant Storage_Array (1 .. Info.Length) := (others => 0);
   begin
      Rs.Append (Empty);
      return Database_Index (Rs.Last_Index);
   end Insert_Record;

   ----------------
   -- Last_Index --
   ----------------

   overriding function Last_Index
     (Store    : Memory_Data_Store;
      Table    : in Table_Index)
      return Database_Index
   is
   begin
      return Database_Index
        (Store.Tables (Positive (Table)).Records.Last_Index);
   end Last_Index;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Current : in out Memory_Data_Store_Cursor) is
   begin
      Key_Maps.Next (Current.Current_Position);
      if Key_Maps.Has_Element (Current.Current_Position) then
         declare
            use type System.Storage_Elements.Storage_Array;
            E : constant System.Storage_Elements.Storage_Array :=
                  Key_Maps.Key (Current.Current_Position);
         begin
            if E > Current.Finish
              or else (Current.Finish_Interval = Open
                       and then E = Current.Finish)
            then
               Current.Current_Position := Key_Maps.No_Element;
            end if;
         end;
      end if;
   end Next;

   ----------
   -- Open --
   ----------

   overriding procedure Open
     (Store  : in out Memory_Data_Store;
      Name   : in     String;
      Magic  : in     Marlowe_Magic_Number)
   is
      use Ada.Streams.Stream_IO;
      File : File_Type;
   begin
      Open (File, In_File, Name);
      Memory_Data_Store'Read (Stream (File), Store);
      Close (File);
      if Store.Magic /= Magic then
         raise Constraint_Error with
           "bad magic number in file " & Name;
      end if;
   end Open;

   --------------
   -- Previous --
   --------------

   overriding procedure Previous (Current : in out Memory_Data_Store_Cursor) is
   begin
      Key_Maps.Previous (Current.Current_Position);
      if Key_Maps.Has_Element (Current.Current_Position) then
         declare
            use type System.Storage_Elements.Storage_Array;
            E : constant System.Storage_Elements.Storage_Array :=
                  Key_Maps.Key (Current.Current_Position);
         begin
            if E < Current.Start
              or else (Current.Start_Interval = Open
                       and then E = Current.Start)
            then
               Current.Current_Position := Key_Maps.No_Element;
            end if;
         end;
      end if;
   end Previous;

   --------------------------
   -- Read_Field_Extension --
   --------------------------

   overriding function Read_Field_Extension
     (Store     : Memory_Data_Store;
      Reference : File_And_Page)
      return System.Storage_Elements.Storage_Array
   is
   begin
      return Store.Extensions (Positive (Reference));
   end Read_Field_Extension;

   -------------------
   -- Record_Length --
   -------------------

   overriding function Record_Length
     (Store  : Memory_Data_Store;
      Table  : in Table_Index)
      return System.Storage_Elements.Storage_Count
   is
   begin
      return Store.Tables (Positive (Table)).Length;
   end Record_Length;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Memory_Data_Store_Cursor) is
   begin
      Item.Current_Position := Key_Maps.No_Element;
      Item.Start := (others => 0);
      Item.Finish := (others => 0);
   end Release;

   ------------
   -- Search --
   ------------

   overriding function Search
     (Store           : Memory_Data_Store;
      Reference       : Key_Reference;
      Start           : System.Storage_Elements.Storage_Array;
      Finish          : System.Storage_Elements.Storage_Array;
      Start_Interval  : Interval_Type;
      Finish_Interval : Interval_Type;
      Scan            : Scan_Type)
      return Data_Store_Cursor
   is
      Result : Memory_Data_Store_Cursor (Reference.Key_Length);
      Keys   : Key_Maps.Map renames
                 Store.Keys (Reference.Key_Index).Map;
   begin
      Result.Start := Start;
      Result.Finish := Finish;
      Result.Start_Interval := Start_Interval;
      Result.Finish_Interval := Finish_Interval;
      case Scan is
         when Forward =>
            Result.Current_Position := Keys.Floor (Start);
            if Key_Maps.Has_Element (Result.Current_Position)
              and then Start_Interval = Open
            then
               Key_Maps.Next (Result.Current_Position);
            end if;
            if Key_Maps.Has_Element (Result.Current_Position) then
               declare
                  use type System.Storage_Elements.Storage_Array;
                  E : constant System.Storage_Elements.Storage_Array :=
                        Key_Maps.Key (Result.Current_Position);
               begin
                  if E > Result.Finish
                    or else (Result.Finish_Interval = Open
                             and then E = Result.Finish)
                  then
                     Result.Current_Position := Key_Maps.No_Element;
                  end if;
               end;
            end if;
         when Backward =>
            Result.Current_Position := Keys.Ceiling (Finish);
            if Key_Maps.Has_Element (Result.Current_Position)
              and then Finish_Interval = Open
            then
               Key_Maps.Previous (Result.Current_Position);
            end if;
            if Key_Maps.Has_Element (Result.Current_Position) then
               declare
                  use type System.Storage_Elements.Storage_Array;
                  E : constant System.Storage_Elements.Storage_Array :=
                        Key_Maps.Key (Result.Current_Position);
               begin
                  if E < Result.Start
                    or else (Result.Start_Interval = Open
                             and then E = Result.Start)
                  then
                     Result.Current_Position := Key_Maps.No_Element;
                  end if;
               end;
            end if;
      end case;
      return Result;
   end Search;

   -----------
   -- Valid --
   -----------

   overriding function Valid
     (Item : Memory_Data_Store_Cursor)
      return Boolean
   is
   begin
      return Key_Maps.Has_Element (Item.Current_Position);
   end Valid;

   -----------------
   -- Valid_Index --
   -----------------

   overriding function Valid_Index
     (Store    : Memory_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index)
      return Boolean
   is
      Info : Table_Info renames Store.Tables (Positive (Table));
      Rs : Record_Vectors.Vector renames Info.Records;
   begin
      return Natural (Db_Index) in 1 .. Rs.Last_Index;
   end Valid_Index;

   ---------------------------
   -- Write_Field_Extension --
   ---------------------------

   overriding procedure Write_Field_Extension
     (Store : in out Memory_Data_Store;
      Reference : File_And_Page;
      Data      : System.Storage_Elements.Storage_Array)
   is
   begin
      Store.Extensions (Positive (Reference)) := Data;
   end Write_Field_Extension;

   ------------------
   -- Write_Record --
   ------------------

   overriding procedure Write_Record
     (Store    : in out Memory_Data_Store;
      Table    : in Table_Index;
      Db_Index : in Database_Index;
      Data     : in System.Address)
   is
      use System.Storage_Elements;
      Info : Table_Info renames Store.Tables (Positive (Table));
      Rs : Record_Vectors.Vector renames Info.Records;
      Rec  : Storage_Array (1 .. Info.Length);
      for Rec'Address use Data;
   begin
      Rs (Positive (Db_Index)) := Rec;
   end Write_Record;

end Marlowe.Data_Stores.Memory;
