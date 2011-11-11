with Ada.Direct_IO;
with Ada.Unchecked_Deallocation;

with Marlowe.Allocation;
with Marlowe.Locks;

package body Marlowe.Files is

   package File_Of_Pages is
      new Ada.Direct_IO (Marlowe.Pages.Page_Record);

   Max_Pages_In_File : constant := 2**24;

   type Array_Of_Files is
     array (File_Index range 1 .. Max_Files) of File_Of_Pages.File_Type;

   type Array_Of_File_Flags is
     array (File_Index range 1 .. Max_Files) of Boolean;

   type Array_Of_File_Locks is
     array (File_Index range 1 .. Max_Files) of Marlowe.Locks.Lock;

   type File_Type_Record is
      record
         File       : Array_Of_Files;
         Open       : Array_Of_File_Flags := (others => False);
         Lock       : Array_Of_File_Locks;
      end record;

   -----------
   -- Clear --
   -----------

   procedure Clear (File     : in File_Type;
                    Location : in File_And_Page)
   is
      use File_Of_Pages;
      Data : Marlowe.Pages.Page_Record;
      pragma Warnings (Off, Data);
      File_Id : constant File_Index := Get_File (Location);
      Page_Id : constant Page_Index := Get_Page (Location);
   begin
      Marlowe.Locks.Exclusive_Lock (File.Lock (File_Id));

      Set_Index (File.File (Get_File (Location)),
                 Positive_Count (Get_Page (Location)));
      Write (File.File (Get_File (Location)), Data);

      pragma Assert (Size (File.File (File_Id)) >= Positive_Count (Page_Id));

      --  We don't actually clear it, for no particular reason.
      --  This function just ensures that the given location
      --  exists, by extending the file if necessary.
      --  Clearing should not be necessary, since it will be
      --  overwritten with new data soon enough.

      Marlowe.Locks.Unlock (File.Lock (File_Id));

   end Clear;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
      use File_Of_Pages;
      procedure Free is
         new Ada.Unchecked_Deallocation (File_Type_Record, File_Type);
   begin
      for I in File.File'Range loop
         if File.Open (I) then
            Close (File.File (I));
         end if;
      end loop;
      Free (File);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File : in out File_Type;
                     Index : in     File_Index;
                     Name : in     String)
   is
      use File_Of_Pages;
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

      Marlowe.Locks.Exclusive_Lock (File.Lock (Index));

      Create (File.File (Index), Inout_File, Name);
      File.Open (Index) := True;

      Marlowe.Locks.Unlock (File.Lock (Index));

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
      return File.Open (Index);
   end Is_Open;

   --------------------
   -- Max_Page_Index --
   --------------------

   function Max_Page_Index (File  : File_Type;
                            Index : File_Index)
                           return Page_Index
   is
      Result : Page_Index;
   begin
      Marlowe.Locks.Shared_Lock (File.Lock (Index));
      Result := Page_Index (File_Of_Pages.Size (File.File (Index)));
      Marlowe.Locks.Shared_Unlock (File.Lock (Index));
      return Result;
   end Max_Page_Index;

   --------------
   -- New_Page --
   --------------

   function New_Page (File  : File_Type;
                      Index : File_Index)
                     return Page_Index
   is
      use File_Of_Pages;
   begin
      pragma Assert (Index > 0);
      pragma Assert (Index < Max_Files);
      pragma Assert (File.Open (Index));

      Marlowe.Locks.Exclusive_Lock (File.Lock (Index));

      if Page_Index (Size (File.File (Index))) >= Max_Pages_In_File then
         Marlowe.Locks.Unlock (File.Lock (Index));
         return 0;
      else
         declare
            Current_Size : constant Count := Size (File.File (Index));
            Data         : Marlowe.Pages.Page_Record;
            pragma Warnings (Off, Data);
            New_Size     : Count;
         begin
            Set_Index (File.File (Index), Current_Size + 1);
            Write (File.File (Index), Data);
            New_Size := Size (File.File (Index));
            Marlowe.Locks.Unlock (File.Lock (Index));
            return Page_Index (New_Size);
         end;
      end if;
   end New_Page;

   ----------
   -- Open --
   ----------

   procedure Open (File : in out File_Type;
                   Index : in     File_Index;
                   Name : in     String)
   is
      use File_Of_Pages;
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

      Open (File.File (Index), Inout_File, Name);
      File.Open (Index) := True;

   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File : in     File_Type;
                   From : in     File_And_Page;
                   Item :    out Pages.Page_Record)
   is
   begin
      Marlowe.Locks.Exclusive_Lock (File.Lock (Get_File (From)));
      pragma Assert (Get_File (From) > 0);
      pragma Assert (Get_File (From) <= Max_Files);
      --  pragma Assert (File.Open (Get_File (From)));
      pragma Assert (Get_Page (From) <= Max_Pages_In_File);
      pragma Assert (Get_Page (From) > 0);
      pragma Assert (File /= null);
      pragma Assert (Get_Page (From) <=
                     Page_Index (File_Of_Pages.Size
                                 (File.File (Get_File (From)))));

      File_Of_Pages.Read (File.File (Get_File (From)),
                          Item,
                          File_Of_Pages.Positive_Count (Get_Page (From)));
      pragma Assert (Pages.Validate (Item));
      Marlowe.Locks.Unlock (File.Lock (Get_File (From)));
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (File : in File_Type;
                    Item : in Pages.Page_Record)
   is
      Location       : constant File_And_Page :=
                         Marlowe.Pages.Get_File_And_Page (Item);
   begin
      Marlowe.Locks.Exclusive_Lock (File.Lock (Get_File (Location)));
      declare
         Last_File_Page : constant Page_Index :=
                            Page_Index (File_Of_Pages.Size
                                        (File.File (Get_File (Location))));
      begin
         pragma Assert (Get_File (Location) > 0);
         pragma Assert (Get_File (Location) <= Max_Files);
         pragma Assert (File.Open (Get_File (Location)));
         pragma Assert (Get_Page (Location) <= Max_Pages_In_File);
         pragma Assert (Get_Page (Location) > 0);
         pragma Assert (File /= null);
         pragma Assert (Get_Page (Location) <= Last_File_Page + 1);

         File_Of_Pages.Write (File.File (Get_File (Location)),
                              Item,
                              File_Of_Pages.Positive_Count
                                (Get_Page (Location)));
         declare
            New_Last_Page : constant Page_Index :=
                              Page_Index (File_Of_Pages.Size
                                          (File.File (Get_File (Location))));
         begin
            null;
            pragma Assert (New_Last_Page = Last_File_Page or else
                             (New_Last_Page = Last_File_Page + 1
                 and then Get_Page (Location) = New_Last_Page));
         end;
      end;

      Marlowe.Locks.Unlock (File.Lock (Get_File (Location)));
   end Write;

end Marlowe.Files;


