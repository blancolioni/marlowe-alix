with Ada.Text_IO;

package body WL.Debug is

   E : array (Debug_Class) of Boolean := (others => False);

   ------------
   -- Enable --
   ------------

   procedure Enable (Name : String) is
   begin
      if Name = "all" then
         E := (others => True);
      elsif Name = "none" then
         E := (others => False);
      else
         declare
            Word  : Boolean  := False;
            Start : Positive;
         begin
            for I in Name'Range loop
               if Name (I) /= ' ' then
                  if not Word then
                     Word := True;
                     Start := I;
                  end if;
               else
                  if Word then
                     begin
                        E (Debug_Class'Value (Name (Start .. I - 1))) :=
                          True;
                     exception
                        when Constraint_Error =>
                           null;
                     end;
                     Word := False;
                  end if;
               end if;
            end loop;

            if Word then
               begin
                  E (Debug_Class'Value (Name (Start .. Name'Last))) :=
                    True;
               exception
                  when Constraint_Error =>
                     null;
               end;
            end if;
         end;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled (Item : Debug_Class) return Boolean is
   begin
      return E (Item);
   end Enabled;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Name : String) is
      Flag : Debug_Class;
   begin
      Flag := Debug_Class'Value (Name);
      E (Flag) := not E (Flag);
   exception
      when Constraint_Error =>
         null;
   end Toggle;

   -----------
   -- Trace --
   -----------

   procedure Trace (When_Debugging : Debug_Class;
                    Show_Message   : String)
   is
   begin
      if E (When_Debugging) then
         Ada.Text_IO.Put_Line (Show_Message);
      end if;
   end Trace;

end WL.Debug;
