with Marlowe.Page_Handles;

package Marlowe.File_Handles is

   type File_Handle is private;

   procedure Create (File : in out File_Handle;
                     Name : in     String);

   procedure Open (File : in out File_Handle;
                   Name : in     String);

   procedure Close (File : in out File_Handle);

   function First_User_Page (File : in File_Handle) return File_And_Page;

   procedure Read (File     : in     File_Handle;
                   Page     : in out Marlowe.Page_Handles.Page_Handle'Class;
                   Location : in     File_And_Page);

   procedure Allocate (File : in     File_Handle;
                       Page : in out Marlowe.Page_Handles.Page_Handle'Class);

   procedure Deallocate (File     : in File_Handle;
                         Location : in File_And_Page);

   procedure Get_Cache_Statistics (File      : in     File_Handle;
                                   Blocks    :    out Natural;
                                   Pages     :    out Natural;
                                   Hits      :    out Natural;
                                   Misses    :    out Natural;
                                   Reads     :    out Natural;
                                   Writes    :    out Natural;
                                   Last_Page : out Page_Index);

private

   type File_Handle_Record;
   type File_Handle is access File_Handle_Record;

end Marlowe.File_Handles;
