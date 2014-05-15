package body Marlowe.Mutex is

   ----------------
   -- Mutex_Type --
   ----------------

   protected body Mutex_Type is

      ------------
      -- S_Lock --
      ------------

      entry S_Lock when not Has_X_Lock is
      begin
         S_Lock_Count := S_Lock_Count + 1;
      end S_Lock;

      --------------
      -- S_Locked --
      --------------

      function S_Locked return Boolean is
      begin
         return S_Lock_Count > 0;
      end S_Locked;

      --------------
      -- S_Unlock --
      --------------

      procedure S_Unlock is
      begin
         S_Lock_Count := S_Lock_Count - 1;
      end S_Unlock;

      ------------
      -- X_Lock --
      ------------

      entry X_Lock when not Has_X_Lock and then S_Lock_Count = 0 is
      begin
         Has_X_Lock := True;
      end X_Lock;

      --------------
      -- X_Locked --
      --------------

      function X_Locked return Boolean is
      begin
         return Has_X_Lock;
      end X_Locked;

      --------------
      -- X_Unlock --
      --------------

      procedure X_Unlock is
      begin
         pragma Assert (Has_X_Lock);
         Has_X_Lock := False;
      end X_Unlock;

   end Mutex_Type;

end Marlowe.Mutex;
