private package Marlowe.Pages.Files is

   procedure Create (Path  : in     String);

   procedure Open (Path  : in String);

   procedure Close;

   procedure Read (From : in File_And_Page;
                   Item : in Page);

   procedure Write (Item : in Page);

   function New_Page return File_And_Page;

end Marlowe.Pages.Files;
