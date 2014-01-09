package body Marlowe.Data_Stores is

   ---------------
   -- Add_Table --
   ---------------

   procedure Add_Table
     (Store    : in out Data_Store_Interface'Class;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
   is
      Result : constant Table_Index := Store.Add_Table (Name, Length);
      pragma Unreferenced (Result);
   begin
      null;
   end Add_Table;

   ------------
   -- Exists --
   ------------

   function Exists
     (Store     : Data_Store_Interface'Class;
      Reference : Key_Reference;
      Key       : System.Storage_Elements.Storage_Array)
      return Boolean
   is
      Position : constant Data_Store_Cursor :=
                   Store.Search (Reference, Key, Key, Closed, Closed, Forward);
   begin
      return Position.Valid;
   end Exists;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (From : Root_Data_Store_Cursor'Class)
      return System.Storage_Elements.Storage_Array
   is
      Result : System.Storage_Elements.Storage_Array (1 .. From.Key_Length);
   begin
      From.Get_Key (Result);
      return Result;
   end Get_Key;

   --------------------
   -- Get_Key_Length --
   --------------------

   function Get_Key_Length
     (Reference : Key_Reference)
      return System.Storage_Elements.Storage_Offset
   is
   begin
      return Reference.Key_Length;
   end Get_Key_Length;

   -------------------------
   -- Get_Key_Table_Index --
   -------------------------

   function Get_Key_Table_Index
     (Reference : Key_Reference)
      return Table_Index
   is
   begin
      return Reference.Table;
   end Get_Key_Table_Index;

   ------------
   -- Search --
   ------------

   function Search
     (Store       : Data_Store_Interface'Class;
      Reference   : Key_Reference;
      Scan        : Scan_Type)
      return Data_Store_Cursor
   is
      Start : constant System.Storage_Elements.Storage_Array
        (1 .. Reference.Key_Length) := (others => 0);
      Finish : constant System.Storage_Elements.Storage_Array
        (1 .. Reference.Key_Length) := (others => 255);
   begin
      return Store.Search
        (Reference       => Reference,
         Start           => Start,
         Finish          => Finish,
         Start_Interval  => Closed,
         Finish_Interval => Closed,
         Scan            => Scan);
   end Search;

end Marlowe.Data_Stores;
