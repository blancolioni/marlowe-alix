with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Text_IO;

with Marlowe.Debug;
with Marlowe.Debug_Classes;

package body Marlowe.Allocation is

   --  Trace_Allocations : constant Boolean := False;

   Allocated : Natural := 0;

   type Allocation_Entry_Record is
      record
         Quantity : Natural := 0;
         Size     : Natural := 0;
      end record;

   type Allocation_Entry is access Allocation_Entry_Record;

   subtype Message_Key is String (1 .. 32);

   function To_Key (Name : String)
                    return Message_Key;

   package Table_Of_Allocations is
     new Ada.Containers.Hashed_Maps
       (Message_Key,
        Allocation_Entry, Ada.Strings.Hash, "=");

   protected Allocation is
      procedure Allocate (Size    : Natural;
                          Message : String);
      procedure Deallocate (Size    : Natural;
                            Message : String);
      procedure Report;
   private
      Allocation_Table : Table_Of_Allocations.Map;
   end Allocation;

   protected body Allocation is

      --------------
      -- Allocate --
      --------------

      procedure Allocate (Size    : Natural;
                          Message : String)
      is
         Bytes : constant Natural := (Size - 1) / 8 + 1;
      begin
         Allocated := Allocated + Bytes;
         declare
            use Table_Of_Allocations;
            Pos   : constant Cursor :=
                      Allocation_Table.Find (To_Key (Message));
         begin
            if Has_Element (Pos) then
               declare
                  Item : constant Allocation_Entry := Element (Pos);
               begin
                  Item.Quantity := Item.Quantity + 1;
                  Item.Size     := Item.Size + Bytes;
               end;
            else
               Allocation_Table.Insert
                 (Key      => To_Key (Message),
                  New_Item => new Allocation_Entry_Record'(1, Bytes));
            end if;
         end;
      end Allocate;

      ----------------
      -- Deallocate --
      ----------------

      procedure Deallocate (Size    : Natural;
                            Message : String)
      is
         Bytes : constant Natural := (Size - 1) / 8 + 1;
      begin
         Allocated := Allocated - Bytes;
         declare
            use Table_Of_Allocations;
            Pos  : constant Cursor :=
                     Allocation_Table.Find (To_Key (Message));
            Item : constant Allocation_Entry :=
                     Element (Pos);
         begin
            Item.Quantity := Item.Quantity - 1;
            Item.Size := Item.Size - Bytes;
         end;
      end Deallocate;

      ------------
      -- Report --
      ------------

      procedure Report is
         use Table_Of_Allocations;
         Pos : Cursor := Allocation_Table.First;
      begin
         while Has_Element (Pos) loop
            declare
               Name : constant String := Key (Pos);
               Item : constant Allocation_Entry := Element (Pos);
            begin
               Ada.Text_IO.Put_Line (Name & ":" &
                                       Natural'Image (Item.Quantity) &
                                       Natural'Image (Item.Size));
            end;
            Next (Pos);
         end loop;
      end Report;

   end Allocation;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Size    : Natural;
                       Message : String)
   is
   begin
      Allocation.Allocate (Size, Message);
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Size    : Natural;
                         Message : String)
   is
   begin
      Allocation.Deallocate (Size, Message);
   end Deallocate;

   ----------------------
   -- Debug_Allocation --
   ----------------------

   function Debug_Allocation return Boolean is
   begin
      return Marlowe.Debug.Enabled (Marlowe.Debug_Classes.Allocation);
   end Debug_Allocation;

   ------------
   -- Report --
   ------------

   procedure Report is
   begin
      Allocation.Report;
   end Report;

   ------------
   -- To_Key --
   ------------

   function To_Key (Name : String)
                    return Message_Key
   is
      Result : Message_Key;
   begin
      if Result'Length <= Name'Length then
         Result := Name (Name'First .. Name'First + Result'Length - 1);
      else
         Result := (others => ' ');
         Result (1 .. Name'Length) := Name;
      end if;
      return Result;
   end To_Key;

end Marlowe.Allocation;
