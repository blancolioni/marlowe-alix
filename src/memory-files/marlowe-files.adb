with Ada.Containers.Vectors;
with Ada.Sequential_IO;
with Ada.Strings.Unbounded;

with Marlowe.Allocation;

package body Marlowe.Files is

   package File_Of_Pages is
      new Ada.Sequential_IO (Marlowe.Pages.Page_Record);

   Max_Pages_In_File   : constant := 2**18;

   type Vector_Page_Index is new Positive;

   package Page_Vectors is
     new Ada.Containers.Vectors (Vector_Page_Index,
                                 Marlowe.Pages.Page_Record,
                                 Marlowe.Pages."=");

   type Memory_File_Record is
      record
         File_Name     : Ada.Strings.Unbounded.Unbounded_String;
         File_Pages    : Page_Vectors.Vector;
         File_Open     : Boolean := False;
      end record;

   type Array_Of_Files is
     array (File_Index range 1 .. Max_Files) of Memory_File_Record;

   type File_Type_Record is
      record
         File       : Array_Of_Files;
      end record;

   -----------
   -- Clear --
   -----------

   procedure Clear (File     : in File_Type;
                    Location : in File_And_Page)
   is
      File_Num   : constant File_Index := Get_File (Location);
      Page_Num   : constant Page_Index := Get_Page (Location);
   begin
      if Vector_Page_Index (Page_Num) >
        File.File (File_Num).File_Pages.Last_Index
      then
         File.File (File_Num).File_Pages.Set_Length
           (Ada.Containers.Count_Type (Page_Num));
      end if;
   end Clear;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
      use File_Of_Pages;
      use Ada.Strings.Unbounded;
      Seq_File : File_Of_Pages.File_Type;
   begin
      for I in File.File'Range loop
         if File.File (I).File_Open then
            Open (Seq_File, Out_File,
                  To_String (File.File (I).File_Name));
            for Page in 1 .. File.File (I).File_Pages.Last_Index loop
               Write (Seq_File, File.File (I).File_Pages.Element (Page));
            end loop;
            Close (Seq_File);
         end if;
      end loop;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File  : in out File_Type;
                     Index : in     File_Index;
                     Name  : in     String)
   is
      use Ada.Strings.Unbounded;
      use File_Of_Pages;
      Seq_File : File_Of_Pages.File_Type;
   begin

      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      pragma Assert (Index > 1 or else File = null);
      pragma Assert (Index = 1 or else File /= null);

      if File = null then
         File := new File_Type_Record;
         if Marlowe.Allocation.Debug_Allocation then
            Marlowe.Allocation.Allocate (File.all'Size, "file-type");
         end if;
      end if;

      Create (Seq_File, Out_File, Name);
      Close (Seq_File);
      File.File (Index).File_Open := True;
      File.File (Index).File_Name := To_Unbounded_String (Name);

   end Create;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File  : File_Type;
                     Index : File_Index)
                    return Boolean
   is
   begin
      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      return File.File (Index).File_Open;
   end Is_Open;

   --------------------
   -- Max_Page_Index --
   --------------------

   function Max_Page_Index (File  : File_Type;
                            Index : File_Index)
                           return Page_Index
   is
   begin
      return Page_Index
        (File.File (Index).File_Pages.Last_Index);
   end Max_Page_Index;

   --------------
   -- New_Page --
   --------------

   function New_Page (File  : File_Type;
                      Index : File_Index)
                     return Page_Index
   is
      use File_Of_Pages;
      Current_Size : constant Page_Index := Max_Page_Index (File, Index);
      New_Size     : constant Page_Index := Current_Size + 1;
   begin
      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      pragma Assert (File.File (Index).File_Open);

      if Current_Size >= Max_Pages_In_File then
         return 0;
      else
         Clear (File, To_File_And_Page (Index, New_Size));
         return New_Size;
      end if;
   end New_Page;

   ----------
   -- Open --
   ----------

   procedure Open (File  : in out File_Type;
                   Index : in     File_Index;
                   Name  : in     String)
   is
      use Ada.Strings.Unbounded;
      use File_Of_Pages;
      Data     : Marlowe.Pages.Page_Record;
      Seq_File : File_Of_Pages.File_Type;
   begin

      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      pragma Assert (Index > 1 or else File = null);
      pragma Assert (Index = 1 or else File /= null);

      if File = null then
         File := new File_Type_Record;
         if Marlowe.Allocation.Debug_Allocation then
            Marlowe.Allocation.Allocate (File.all'Size, "file-type");
         end if;
      end if;

      Open (Seq_File, In_File, Name);
      while not End_Of_File (Seq_File) loop
         Read (Seq_File, Data);
         File.File (Index).File_Pages.Append (Data);
      end loop;
      Close (Seq_File);
      File.File (Index).File_Open := True;
      File.File (Index).File_Name := To_Unbounded_String (Name);

   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File : in     File_Type;
                   From : in     File_And_Page;
                   Item :    out Pages.Page_Record)
   is
   begin
      pragma Assert (Get_File (From) > 0);
      pragma Assert (Get_File (From) <= Max_Files);
      --  pragma Assert (File.Open (Get_File (From)));
      pragma Assert (Get_Page (From) <= Max_Pages_In_File);
      pragma Assert (Get_Page (From) > 0);
      pragma Assert (File /= null);
      Item :=
        File.File (Get_File (From)).File_Pages.Element
        (Vector_Page_Index (Get_Page (From)));
      pragma Assert (Pages.Validate (Item));
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (File : in File_Type;
                    Item : in Pages.Page_Record)
   is
      Location : constant File_And_Page :=
        Marlowe.Pages.Get_File_And_Page (Item);
   begin
      pragma Assert (Get_File (Location) > 0);
      pragma Assert (Get_File (Location) <= Max_Files);
      pragma Assert (File.File (Get_File (Location)).File_Open);
      pragma Assert (Get_Page (Location) <= Max_Pages_In_File);
      pragma Assert (Get_Page (Location) > 0);
      pragma Assert (File /= null);
      Clear (File, Location);
      File.File (Get_File (Location)).File_Pages.Replace_Element
        (Vector_Page_Index (Get_Page (Location)),
         Item);

   end Write;

end Marlowe.Files;


