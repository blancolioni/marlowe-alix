package body Marlowe.Locks is

   --------------------
   -- Exclusive_Lock --
   --------------------

   procedure Exclusive_Lock (Item : in out Lock) is
   begin
      Item.Exclusive_Lock;
   end Exclusive_Lock;

   ----------------------
   -- Exclusive_Locked --
   ----------------------

   function Exclusive_Locked (Item : in Lock) return Boolean is
   begin
      return Item.Exclusive_Locked;
   end Exclusive_Locked;

   -----------------
   -- Shared_Lock --
   -----------------

   procedure Shared_Lock    (Item : in out Lock) is
   begin
      Item.Shared_Lock;
   end Shared_Lock;

   -----------------------
   -- Shared_Lock_Count --
   -----------------------

   function Shared_Lock_Count (Item : Lock) return Natural is
   begin
      return Item.Shared_Lock_Count;
   end Shared_Lock_Count;

   -------------------
   -- Shared_Locked --
   -------------------

   function Shared_Locked    (Item : in Lock) return Boolean is
   begin
      return Item.Shared_Locked;
   end Shared_Locked;

   -------------------
   -- Shared_Unlock --
   -------------------

   procedure Shared_Unlock    (Item : in out Lock) is
   begin
      Item.Shared_Unlock;
   end Shared_Unlock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : in out Lock) is
   begin
      Item.Unlock;
   end Unlock;

   -----------------
   -- Update_Lock --
   -----------------

   procedure Update_Lock (Item : in out Lock) is
   begin
      Item.Update_Lock;
   end Update_Lock;

   -------------------
   -- Update_Locked --
   -------------------

   function Update_Locked    (Item : in Lock) return Boolean is
   begin
      return Item.Update_Locked;
   end Update_Locked;

   --------------------------
   -- Upgrade_To_Exclusive --
   --------------------------

   procedure Upgrade_To_Exclusive (Item : in out Lock) is
   begin
      Item.Upgrade_To_Exclusive;
   end Upgrade_To_Exclusive;

   ----------
   -- Lock --
   ----------

   protected body Lock is

      entry Exclusive_Lock
        when not Is_Exclusive_Locked
        and then not Is_Update_Locked
        and then Num_Shared_Locks = 0
      is
      begin
         Is_Exclusive_Locked := True;
      end Exclusive_Lock;

      function Exclusive_Locked return Boolean is
      begin
         return Is_Exclusive_Locked;
      end Exclusive_Locked;

      entry Shared_Lock
        when not Is_Exclusive_Locked
      is
      begin
         Num_Shared_Locks := Num_Shared_Locks + 1;
      end Shared_Lock;

      function Shared_Lock_Count return Natural is
      begin
         return Num_Shared_Locks;
      end Shared_Lock_Count;

      function Shared_Locked return Boolean is
      begin
         return Num_Shared_Locks > 0;
      end Shared_Locked;

      procedure Shared_Unlock is
      begin
         Num_Shared_Locks := Num_Shared_Locks - 1;
      end Shared_Unlock;

      procedure Unlock is
      begin
         if Is_Exclusive_Locked then
            Is_Exclusive_Locked := False;
         elsif Is_Update_Locked then
            Is_Update_Locked := False;
         else
            raise Constraint_Error;
         end if;
      end Unlock;

      entry Update_Lock when
        not Is_Exclusive_Locked and not Is_Update_Locked
      is
      begin
         Is_Update_Locked := True;
      end Update_Lock;

      function Update_Locked return Boolean is
      begin
         return Is_Update_Locked;
      end Update_Locked;

      entry Upgrade_To_Exclusive when Num_Shared_Locks = 0 is
      begin
         Is_Exclusive_Locked := True;
         Is_Update_Locked := False;
      end Upgrade_To_Exclusive;

   end Lock;

end Marlowe.Locks;
