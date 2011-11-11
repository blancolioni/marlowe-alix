generic
   type Debug_Class is (<>);
package WL.Debug is

   function Enabled (Item : Debug_Class) return Boolean;
   procedure Enable (Name : String);
   procedure Toggle (Name : String);

   procedure Trace (When_Debugging : Debug_Class;
                    Show_Message   : String);

end WL.Debug;
