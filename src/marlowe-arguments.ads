package Marlowe.Arguments is

   function Flag_Set (Long_Name     : String;
                      Short_Name    : String;
                      Default_Value : Boolean)
                      return Boolean;

   function Option_Value (Long_Name     : String;
                          Short_Name    : String;
                          Default_Value : String)
                          return String;

end Marlowe.Arguments;
