package Marlowe.Mutex is

   protected type Mutex_Type is
      function S_Locked return Boolean;
      function X_Locked return Boolean;
      entry S_Lock;
      procedure S_Unlock;
      entry X_Lock;
      procedure X_Unlock;
   private
      Has_X_Lock : Boolean := False;
      S_Lock_Count : Natural := 0;
   end Mutex_Type;

end Marlowe.Mutex;
