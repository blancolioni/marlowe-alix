with Marlowe.Pages;

package Marlowe.Files is

   Max_Files : constant := 64;

   type File_Type is private;

   procedure Create (File  : in out File_Type;
                     Index : in     File_Index;
                     Name  : in     String);

   procedure Open (File  : in out File_Type;
                   Index : in     File_Index;
                   Name  : in     String);

   procedure Close (File : in out File_Type);

   procedure Read (File : in     File_Type;
                   From : in     File_And_Page;
                   Item :    out Pages.Page_Record);

   procedure Write (File : in File_Type;
                    Item : in Pages.Page_Record);

   function New_Page (File  : File_Type;
                      Index : File_Index)
                     return Page_Index;

   function Max_Page_Index (File  : File_Type;
                            Index : File_Index)
                           return Page_Index;

   procedure Clear (File     : in File_Type;
                    Location : in File_And_Page);
   --  Clear a page for (re-)use; extends the file if necessary

   function Is_Open (File  : File_Type;
                     Index : File_Index)
                    return Boolean;

   procedure Get_Statistics
     (File   : File_Type;
      Last   : out Page_Index;
      Reads  : out Natural;
      Writes : out Natural);

private

   type File_Type_Record;

   type File_Type is access File_Type_Record;

end Marlowe.Files;
