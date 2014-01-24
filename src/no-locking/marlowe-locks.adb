package body Marlowe.Locks is

   --------------------
   -- Exclusive_Lock --
   --------------------

   procedure Exclusive_Lock (Item : in out Lock) is
   begin
      Item.Is_Exclusive_Locked := True;
   end Exclusive_Lock;

   ----------------------
   -- Exclusive_Locked --
   ----------------------

   function Exclusive_Locked (Item : in Lock) return Boolean is
   begin
      return Item.Is_Exclusive_Locked;
   end Exclusive_Locked;

   -----------------
   -- Shared_Lock --
   -----------------

   procedure Shared_Lock    (Item : in out Lock) is
   begin
      Item.Num_Shared_Locks :=
        Item.Num_Shared_Locks + 1;
   end Shared_Lock;

   -----------------------
   -- Shared_Lock_Count --
   -----------------------

   function Shared_Lock_Count (Item : Lock) return Natural is
   begin
      return Item.Num_Shared_Locks;
   end Shared_Lock_Count;

   -------------------
   -- Shared_Locked --
   -------------------

   function Shared_Locked    (Item : in Lock) return Boolean is
   begin
      return Item.Num_Shared_Locks > 0;
   end Shared_Locked;

   -------------------
   -- Shared_Unlock --
   -------------------

   procedure Shared_Unlock    (Item : in out Lock) is
   begin
      Item.Num_Shared_Locks := Item.Num_Shared_Locks - 1;
   end Shared_Unlock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : in out Lock) is
   begin
      Item.Is_Exclusive_Locked := False;
   end Unlock;

   -----------------
   -- Update_Lock --
   -----------------

   procedure Update_Lock (Item : in out Lock) is
   begin
      Item.Is_Update_Locked := True;
   end Update_Lock;

   -------------------
   -- Update_Locked --
   -------------------

   function Update_Locked    (Item : in Lock) return Boolean is
   begin
      return Item.Is_Update_Locked;
   end Update_Locked;

   --------------------------
   -- Upgrade_To_Exclusive --
   --------------------------

   procedure Upgrade_To_Exclusive (Item : in out Lock) is
   begin
      Item.Is_Exclusive_Locked := True;
      Item.Is_Update_Locked := False;
   end Upgrade_To_Exclusive;

end Marlowe.Locks;
