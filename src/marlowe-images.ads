with System.Storage_Elements;

package Marlowe.Images is

   function Storage_Array_Image
     (Item : System.Storage_Elements.Storage_Array)
     return String;
   --  Return a string containing a string of hexadecimal digits
   --  corresponding to the storage array

end Marlowe.Images;
