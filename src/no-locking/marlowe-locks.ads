package Marlowe.Locks is

   type Lock is limited private;

   procedure Shared_Lock    (Item : in out Lock);
   procedure Update_Lock    (Item : in out Lock);
   procedure Exclusive_Lock (Item : in out Lock);

   procedure Upgrade_To_Exclusive (Item : in out Lock);

   procedure Shared_Unlock (Item : in out Lock);
   procedure Unlock (Item : in out Lock);
   --  Shared_Unlock only unlocks shared locks
   --  Unlock only unlocks update or exclusive locks.

   function Exclusive_Locked (Item : in Lock) return Boolean;
   function Update_Locked    (Item : in Lock) return Boolean;
   function Shared_Locked    (Item : in Lock) return Boolean;

   function Shared_Lock_Count (Item : Lock) return Natural;

private

   type Lock is
      record
         Num_Shared_Locks    : Natural := 0;
         Is_Update_Locked    : Boolean := False;
         Is_Exclusive_Locked : Boolean := False;
      end record;

end Marlowe.Locks;
