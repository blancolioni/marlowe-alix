package Marlowe.Allocation is

   procedure Allocate (Size    : Natural;
                       Message : String);

   procedure Deallocate (Size    : Natural;
                         Message : String);

   procedure Report;

   function Debug_Allocation return Boolean;

end Marlowe.Allocation;

