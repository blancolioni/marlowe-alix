with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Marlowe.Allocation;
with Marlowe.Configuration;
with Marlowe.Page_Handles;
with Marlowe.Pages;
with Marlowe.Tables;

with Marlowe.Handles.Data_Page_Handle;
with Marlowe.Handles.Field_Extension_Page_Handles;
with Marlowe.Handles.Page_Pointer_Page_Handle;
with Marlowe.Handles.Storage_Element_Page_Handle;

with Marlowe.Btree_Header_Page_Handles;
with Marlowe.Btree.Data_Definition_Handles;
with Marlowe.Key_Storage;

with Marlowe.File_Handles;

with Marlowe.Debug_Classes;
with Marlowe.Debug;

with Marlowe.Trace;

package body Marlowe.Btree_Handles is

   Max_Root_Pages : constant := 6;

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists (Btree_Reference,
                                             Marlowe.Data_Stores."=");

   type Table_Reference is
      record
         Record_Index           : Table_Index;
         Record_Root            : File_And_Page;
         Page_Pointers_Per_Page : Slot_Index;
         Db_Records_Per_Page    : Natural;
      end record;

   package Table_Reference_Vectors is
      new Ada.Containers.Vectors (Positive, Table_Reference);

   type Btree_Handle_Record is
      record
         File       : File_Handles.File_Handle;
         Header     : Btree_Header_Page_Handles.Btree_Header_Page_Handle;
         Table_Root : Btree.Data_Definition_Handles.Data_Definition_Handle;
         Refs       : List_Of_References.List;
         Table_Refs : Table_Reference_Vectors.Vector;
      end record;

   function Consistent (Current : Btree_Mark) return Boolean;
   --  Return true if Current is self-consistent; i.e. if its
   --  position obeys its constraints.

   function Empty_Mark
     (Key_Length : System.Storage_Elements.Storage_Count)
      return Btree_Mark;

   --  Local function to return a mark which indicates a search failure

   function Debug_Delete return Boolean;
   pragma Unreferenced (Debug_Delete);

   function Create_Table_Reference
     (Index                  : Table_Index;
      Length                 : Storage_Count;
      Root_Page              : File_And_Page)
      return Table_Reference;

   function Get_Record_Address (Handle   : in Btree_Handle;
                                Index    : in Table_Index;
                                Db_Index : in Database_Index)
                               return File_Page_And_Slot;

   type Array_Of_Slots is array (Positive range <>) of Slot_Index;

   function Get_Slot_Path (Page_Pointers_Per_Page : Database_Index;
                           Db_Records_Per_Page    : Database_Index;
                           Db_Index               : Database_Index)
                           return Array_Of_Slots;

   function Get_Data_Definition
     (Handle : Btree_Handle;
      Index  : Table_Index)
      return Marlowe.Btree.Data_Definition_Handles.Data_Definition_Handle;

   procedure Trace (Message : String);

   -------------
   -- Add_Key --
   -------------

   function Add_Key
     (Handle       : in    Btree_Handle;
      Name         : in    String;
      Record_No    : in    Table_Index;
      Length       : in    System.Storage_Elements.Storage_Offset)
      return Btree_Reference
   is
      New_Ref     : Btree_Reference;
      Index       : Natural := 0;
      Local_Index : Positive;
      Header      : Btree_Header_Page_Handles.Btree_Header_Page_Handle :=
        Handle.Header;
   begin

      declare
         use Marlowe.Btree_Header_Page_Handles;
      begin
         while Number_Of_Btrees (Header) = Maximum_Btree_Count and then
           Get_Overflow_Page (Header) /= 0
         loop
            Header := Get_Overflow_Page (Header);
            Index  := Index + Maximum_Btree_Count;
         end loop;

         if Number_Of_Btrees (Header) >= Maximum_Btree_Count then
            declare
               Overflow : Btree_Header_Page_Handle;
            begin
               Marlowe.File_Handles.Allocate (Handle.File, Overflow);
               Set_Overflow_Page (Header, Get_Location (Overflow));
               Index := Index + Maximum_Btree_Count;
               Header := Overflow;
            end;
         end if;

         Local_Index := Add_Btree_Description (Header, Name, Length);

         Index := Index + Local_Index;

         Increment_Total_Btrees (Handle.Header);

      end;

      declare
         Root : Marlowe.Btree_Page_Handles.Btree_Page_Handle;
      begin
         Marlowe.File_Handles.Allocate
           (Handle.File,
            Page_Handles.Page_Handle'Class (Root));

         Marlowe.Btree_Page_Handles.Set_Page_Info
           (Root, True, Integer (Length));

         Btree_Header_Page_Handles.Set_Btree_Root
           (Header, Local_Index,
            Marlowe.Btree_Page_Handles.Get_Location (Root));
         Btree_Page_Handles.Unlock (Root);
      end;

      New_Ref.Name       := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      New_Ref.Key_Index  := Index;
      New_Ref.Table      := Record_No;
      New_Ref.Key_Length := Length;
      List_Of_References.Append (Handle.Refs, New_Ref);

      return New_Ref;

   end Add_Key;

   ---------------
   -- Add_Table --
   ---------------

   function Add_Table (Handle   : in Btree_Handle;
                       Name     : in String;
                       Length   : in Storage_Count)
                      return Table_Index
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Index  : Natural := 0;
      Root   : Marlowe.Handles.Page_Pointer_Page_Handle.Table_Page_Handle;
      Direct : Marlowe.Handles.Data_Page_Handle.Table_Page_Handle;
      Max    : constant Natural := Maximum_Data_Record_Count;
      Table  : Data_Definition_Handle := Handle.Table_Root;
   begin

      --  Find a table page that is either not full, or has
      --  no overflow page.

      Table.Exclusive_Lock;
      while Table.Number_Of_Data_Records = Max
        and then Table.Get_Overflow_Page /= 0
      loop
         declare
            Overflow : constant Data_Definition_Handle :=
                         Table.Get_Overflow_Page;
         begin
            Overflow.Exclusive_Lock;
            Table.Unlock;
            Table := Overflow;
            Index := Index + Max;
         end;
      end loop;

      if Table.Number_Of_Data_Records = Max then

         --  Page is full, which means (by the while loop condition)
         --  it doesn't have an overflow page.  We create a new one now.

         declare
            Overflow : Data_Definition_Handle;
         begin
            Marlowe.File_Handles.Allocate (Handle.File, Overflow);
            Set_Overflow_Page (Table, Overflow.Get_Location);
            Index := Index + Max;
            Overflow.Set_First_Table_Index (Table_Index (Index));
            Table.Unlock;
            Table := Overflow;
         end;
      end if;

      --  Add the table definition, and allocate a table root page
      --  and an initial direct pointer page.

      declare
         Local_Index : constant Table_Index :=
                         Table.Add_Record_Definition (Name, Length);
         Result_Index : constant Table_Index :=
                          Table_Index (Index) + Local_Index;
      begin
         Marlowe.File_Handles.Allocate (Handle.File, Root);
         Marlowe.File_Handles.Allocate (Handle.File, Direct);

         Root.Set_Table_Value (1, Direct.Get_Location);
         Table.Set_Record_Root (Local_Index, Root.Get_Location);

         Handle.Table_Refs.Append
           (Create_Table_Reference (Index     => Result_Index,
                                    Length    => Length,
                                    Root_Page => Root.Get_Location));

         Root.Unlock;
         Direct.Unlock;
         Table.Unlock;

         return Result_Index;
      end;

   end Add_Table;

   ---------------
   -- Add_Table --
   ---------------

   procedure Add_Table
     (Handle   : in Btree_Handle;
      Name     : in String;
      Length   : in System.Storage_Elements.Storage_Count)
   is
      Unused : constant Table_Index :=
        Add_Table (Handle, Name, Length);
      pragma Unreferenced (Unused);
   begin
      null;
   end Add_Table;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Mark : in out Btree_Mark) is
      use Marlowe.Btree_Page_Handles;
   begin
      if Mark.Slot /= 0 then
--           Ada.Text_IO.Put_Line ("Adjusting mark: " &
--                                 Image (Mark.Page.Get_Location));
         Btree_Page_Handles.Shared_Lock (Mark.Page);
      end if;
   end Adjust;

   ----------------
   -- Check_Tree --
   ----------------

   function Check_Tree (Handle    : Btree_Handle;
                        Reference : Btree_Reference)
                       return Boolean
   is

      use Marlowe.Btree_Page_Handles;

      OK : Boolean := True;

      procedure Check_Page (P : Btree_Page_Handle);
      --  Checks an individual page, and calls itself recursively
      --  on the page's children

      ----------------
      -- Check_Page --
      ----------------

      procedure Check_Page (P : Btree_Page_Handle) is
         use Marlowe.Key_Storage;
         K1, K2 : System.Storage_Elements.Storage_Array
           (1 .. System.Storage_Elements.Storage_Offset (Get_Key_Length (P)));
      begin
         if Is_Leaf (P) then
            null;
         else
            for I in Slot_Index range 1 .. Number_Of_Keys (P) + 1 loop
               declare
                  Child : constant Btree_Page_Handle := Get_Child (P, I);
               begin
                  if Get_Parent (Child) /= Get_Location (P) then
                     Ada.Text_IO.Put_Line
                       ("Child " &
                          Image (Get_Location (Child)) &
                          " of page " &
                          Image (Get_Location (P)) &
                          " thinks that its parent is " &
                          Image (Get_Parent (Child)));
                     OK := False;
                  end if;
                  if Number_Of_Keys (Child) < Minimum_Keys (Child) then
                     Ada.Text_IO.Put_Line
                       ("Child " &
                          Image (Get_Location (Child)) &
                          " of page " &
                          Image (Get_Location (P)) &
                          " has only " &
                          Slot_Index'Image (Number_Of_Keys (Child)) &
                          " keys");
                     OK := False;
                  end if;

                  if I <= Number_Of_Keys (P) then
                     for J in Slot_Index
                       range 1 .. Number_Of_Keys (Child)
                     loop
                        Get_Key (P, I, K1);
                        Get_Key (Child, J, K2);

                        case Compare (K1, K2) is
                           when Less =>
                              Ada.Text_IO.Put_Line
                                ("Child " &
                                   Image (Get_Location (Child)) &
                                   " of page " &
                                   Image (Get_Location (P)) &
                                   " contains a key greater than " &
                                   "its parent slot");
                              OK := False;
                           when Equal =>
                              Ada.Text_IO.Put_Line
                                ("Child " &
                                   Image (Get_Location (Child)) &
                                   " of page " &
                                   Image (Get_Location (P)) &
                                   " contains a key equal to its parent slot");
                              OK := False;
                           when Greater =>
                              null;
                        end case;
                     end loop;
                  end if;

                  Check_Page (Child);
               end;
            end loop;
         end if;
      end Check_Page;
   begin
      Check_Page (Get_Root (Handle, Reference));
      return OK;
   end Check_Tree;

   -----------
   -- Close --
   -----------

   procedure Close (Handle : in out Btree_Handle) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Btree_Handle_Record,
                                         Btree_Handle);
      File : File_Handles.File_Handle := Handle.File;
   begin
      --  Freeing the handle causes Finalization on the header
      --  page which needs the cache to be around, so we do that
      --  first before closing the file (which destroys the cache).
      Free (Handle);
      File_Handles.Close (File);
   end Close;

   ----------------
   -- Consistent --
   ----------------

   function Consistent (Current : Btree_Mark) return Boolean is
   begin
      if not Valid (Current) then
         return False;
      elsif Current.Key_Length > 0 then
         declare
            use Marlowe.Btree_Page_Handles;
            Page : Btree_Page_Handle renames Current.Page;
            Key  : System.Storage_Elements.Storage_Array
              (1 .. Current.Key_Length);
            --              Defn : constant Component_Array :=
            --                Get_Key_Definition (Current.Reference);
         begin
            Get_Key (Page, Current.Slot, Key);
            return Current.Finish_Interval = Unbounded
              or else (Key = Current.Start_Key and then
                         Current.Start_Interval = Closed)
              or else (Key = Current.Finish_Key and then
                         Current.Finish_Interval = Closed)
              or else (Current.Start_Key < Key
                       and then Current.Finish_Key > Key);
         end;
      else
         return True;
      end if;
   end Consistent;

   ------------
   -- Create --
   ------------

   procedure Create (Handle : in out Btree_Handle;
                     Name   : in     String;
                     Magic  : in     Marlowe_Magic_Number)
   is
      use Marlowe.Btree_Header_Page_Handles;
      use Marlowe.Btree.Data_Definition_Handles;
   begin

      Handle := new Btree_Handle_Record;

      if Marlowe.Allocation.Debug_Allocation then
         Marlowe.Allocation.Allocate (Handle.all'Size, "btree-handle");
      end if;

      Marlowe.File_Handles.Create (Handle.File,
                                   Marlowe.Configuration.Working_Directory &
                                     Name);
      Marlowe.File_Handles.Allocate
        (Handle.File,
         Page_Handles.Page_Handle'Class (Handle.Header));
      pragma Assert (Get_Location (Handle.Header) =
                       File_Handles.First_User_Page (Handle.File));
      Marlowe.File_Handles.Allocate
        (Handle.File,
         Page_Handles.Page_Handle'Class (Handle.Table_Root));
      Handle.Header.Set_Table_Page (Handle.Table_Root.Get_Location);
      Marlowe.Btree_Header_Page_Handles.Set_Magic
        (Handle.Header,
         Magic);
      Btree_Header_Page_Handles.Unlock (Handle.Header);
      Marlowe.Btree.Data_Definition_Handles.Unlock (Handle.Table_Root);
   end Create;

   ----------------------------
   -- Create_Field_Extension --
   ----------------------------

   function Create_Field_Extension
     (Handle : Btree_Handle)
      return File_And_Page
   is
      use Marlowe.Handles.Field_Extension_Page_Handles;
      Extension_Handle : Field_Extension_Page_Handle;
      Result : File_And_Page;
   begin
      Marlowe.File_Handles.Allocate
        (Handle.File,
         Page_Handles.Page_Handle'Class (Extension_Handle));
      Result := Extension_Handle.Get_Location;
      Extension_Handle.Unlock;
      return Result;
   end Create_Field_Extension;

   ----------------------------
   -- Create_Table_Reference --
   ----------------------------

   function Create_Table_Reference
     (Index                  : Table_Index;
      Length                 : Storage_Count;
      Root_Page              : File_And_Page)
      return Table_Reference
   is
      Result : Table_Reference;
      Db_Records_Per_Page  : constant Database_Index :=
                               Database_Index
                                 (Marlowe.Btree.Records_Per_Page (Length));
      Page_Pointers_Per_Page : constant Database_Index :=
                                 Database_Index
                                   (Marlowe.Pages.Elements_Per_Page
                                      (File_And_Page_Bits /
                                         System.Storage_Unit));
   begin
      Result.Record_Index := Index;
      Result.Record_Root  := Root_Page;
      Result.Db_Records_Per_Page :=
        Natural (Db_Records_Per_Page);
      Result.Page_Pointers_Per_Page :=
        Slot_Index (Page_Pointers_Per_Page);
      return Result;
   end Create_Table_Reference;

   ------------------
   -- Debug_Delete --
   ------------------

   function Debug_Delete return Boolean is
   begin
      return Marlowe.Debug.Enabled (Marlowe.Debug_Classes.Deletion);
   end Debug_Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Handle     : in Btree_Handle;
                     Reference  : in Btree_Reference;
                     Key        : in System.Storage_Elements.Storage_Array)
   is
      use Marlowe.Btree_Page_Handles;

      subtype Key_Type is
        System.Storage_Elements.Storage_Array (1 .. Reference.Key_Length);

      procedure Do_Delete (Page : in Btree_Page_Handle;
                           K    : in System.Storage_Elements.Storage_Array);
      --  Delete Key, starting from Page.
      --  Key must be either in Page, or in one of Page's children

      procedure Delete_Internal
        (Page   : in Btree_Page_Handle;
         Index  : in Slot_Index;
         K      : in System.Storage_Elements.Storage_Array);
      --  Delete a key from an internal page

      procedure Delete_From_Subtree
        (Page   : in Btree_Page_Handle;
         Index  : in Slot_Index;
         K      : in System.Storage_Elements.Storage_Array);
      --  Delete a key from a subtree

      -------------------------
      -- Delete_From_Subtree --
      -------------------------

      procedure Delete_From_Subtree
        (Page   : in Btree_Page_Handle;
         Index  : in Slot_Index;
         K      : in System.Storage_Elements.Storage_Array)
      is
         Child : Btree_Page_Handle := Get_Child (Page, Index);
      begin

         if Marlowe.Trace.Tracing then
            Trace ("Delete_From_Subtree: page"
                   & Image (Get_Location (Page))
                   & "; index" & Index'Img
                   & "; key "
                   & Marlowe.Key_Storage.Image (K));
         end if;

         if Number_Of_Keys (Child) > Minimum_Keys (Child) then
            Trace ("Case 3");
            Do_Delete (Child, K);
         elsif Index > 1 and then
           Number_Of_Keys (Get_Child (Page, Index - 1)) >
           Minimum_Keys (Child)
         then
            Trace ("Case 3a (i)");
            declare
               Sib_Key, Last_Sib_Key : Key_Type;
               Sib_Page       : constant Btree_Page_Handle :=
                 Get_Child (Page, Index - 1);
               Last_Sib_Child : constant File_And_Page :=
                 Get_Child (Sib_Page, Number_Of_Keys (Sib_Page) + 1);
            begin
               Get_Key (Page, Index - 1, Sib_Key);
               Get_Key (Sib_Page, Number_Of_Keys (Sib_Page),
                        Last_Sib_Key);

               Delete_Key (Page, Index - 1);
               Insert_Key (Child, Sib_Key, Last_Sib_Child);

               if Is_Leaf (Sib_Page) then
                  Insert_Key (Page, Last_Sib_Key,
                              Get_Location (Sib_Page));
                  Delete_Key (Sib_Page, Number_Of_Keys (Sib_Page));
               else
                  declare
                     Last_Sib_Child_Handle : Btree_Page_Handle;
                     Last_Sib_Page_Key : Key_Type;
                  begin
                     Derive (Sib_Page, Last_Sib_Child_Handle);
                     Set_Page (Last_Sib_Child_Handle, Last_Sib_Child);
                     Set_Parent (Last_Sib_Child_Handle, Child);
                     Get_Key (Sib_Page, Number_Of_Keys (Sib_Page),
                              Last_Sib_Page_Key);
                     Insert_Key (Page, Last_Sib_Page_Key,
                                 Get_Location (Sib_Page));
                     Delete_Key (Sib_Page, Number_Of_Keys (Sib_Page) + 1);
                  end;
               end if;

               Do_Delete (Child, K);
            end;
         elsif Index <= Number_Of_Keys (Page) and then
           Number_Of_Keys (Get_Child (Page, Index + 1)) >
           Minimum_Keys (Get_Child (Page, Index + 1))
         then
            Trace ("Case 3a (ii)");
            declare
               Sib_Key, First_Sib_Key : Key_Type;
               Sib_Page : constant Btree_Page_Handle :=
                 Get_Child (Page, Index + 1);
               First_Sib_Child : constant File_And_Page :=
                 Get_Child (Sib_Page, 1);
            begin
               Get_Key (Page, Index, Sib_Key);
               Get_Key (Sib_Page, 1, First_Sib_Key);

               Delete_Key (Page, Index);

               if Is_Leaf (Child) then
                  Insert_Key (Child, Sib_Key);
               else
                  Set_Number_Of_Keys (Child, Number_Of_Keys (Child) + 1);
                  Set_Key (Child, Number_Of_Keys (Child), Sib_Key);
                  Set_Child (Child, Number_Of_Keys (Child) + 1,
                             First_Sib_Child);
               end if;

               Delete_Key (Sib_Page, 1);
               Insert_Key (Page, First_Sib_Key, Get_Location (Child));
               if not Is_Leaf (Sib_Page) then
                  declare
                     First_Sib_Child_Handle : Btree_Page_Handle;
                  begin
                     Derive (Sib_Page, First_Sib_Child_Handle);
                     Set_Page (First_Sib_Child_Handle, First_Sib_Child);
                     Set_Parent (First_Sib_Child_Handle, Child);
                  end;
               end if;

               Do_Delete (Child, K);

            end;
         elsif Index > 1 then
            Trace ("Case 3b");
            declare
               Left_Key   : Key_Type;
               Left_Child : constant Btree_Page_Handle :=
                 Get_Child (Page, Index - 1);
               Left_Keys  : constant Slot_Index := Number_Of_Keys (Left_Child);
            begin

               Get_Key (Page, Index - 1, Left_Key);
               Delete_Key (Page, Index - 1);

               if Is_Leaf (Left_Child) then
                  Insert_Key (Child, Left_Key);
               else
                  Set_Parent (Get_Child (Left_Child, Left_Keys + 1),
                              Child);
                  Insert_Key (Child, Left_Key,
                              Get_Child (Left_Child, Left_Keys + 1));
               end if;

               declare
                  Child_Key : Key_Type;
               begin
                  for I in 1 .. Left_Keys loop
                     Get_Key (Left_Child, I, Child_Key);
                     if not Is_Leaf (Left_Child) then
                        Set_Parent (Get_Child (Left_Child, I), Child);
                        Insert_Key (Child, Child_Key,
                                    Get_Child (Left_Child, I));
                     else
                        Insert_Key (Child, Child_Key);
                     end if;
                  end loop;
               end;

               Do_Delete (Child, K);

            end;
         else
            Trace ("Case 3c");
            declare
               Left_Key   : Key_Type;
               Left_Child : constant Btree_Page_Handle :=
                 Get_Child (Page, Index);
               Left_Keys  : constant Slot_Index  :=
                 Number_Of_Keys (Left_Child);
            begin

               Get_Key (Page, Index, Left_Key);
               Child := Get_Child (Page, Index + 1);

               Delete_Key (Page, Index);

               if Is_Leaf (Left_Child) then
                  Insert_Key (Child, Left_Key);
               else
                  Set_Parent (Get_Child (Left_Child, Left_Keys + 1),
                              Child);
                  Insert_Key (Child, Left_Key,
                          Get_Child (Left_Child, Left_Keys + 1));
               end if;

               declare
                  Child_Key : Key_Type;
               begin
                  for I in 1 .. Number_Of_Keys (Left_Child) loop
                     Get_Key (Left_Child, I, Child_Key);
                     if not Is_Leaf (Left_Child) then
                        Set_Parent (Get_Child (Left_Child, I), Child);
                        Insert_Key (Child, Child_Key,
                                    Get_Child (Left_Child, I));
                     else
                        Insert_Key (Child, Child_Key);
                     end if;
                  end loop;
               end;

               Do_Delete (Child, K);

            end;
         end if;
      end Delete_From_Subtree;

      ---------------------
      -- Delete_Internal --
      ---------------------

      procedure Delete_Internal
        (Page   : in Btree_Page_Handle;
         Index  : in Slot_Index;
         K      : in System.Storage_Elements.Storage_Array)
      is
         Left     : constant Btree_Page_Handle := Get_Child (Page, Index);
         Right    : constant Btree_Page_Handle := Get_Child (Page, Index + 1);
         Sib      : Key_Type;
         Temp_Key : System.Storage_Elements.Storage_Array (K'Range);
      begin
         if Number_Of_Keys (Left) > Minimum_Keys (Left) then
            Trace ("Case 2a");
            declare
               P : Btree_Page_Handle := Left;
            begin
               while not Is_Leaf (P) loop
                  P := Get_Child (P, Number_Of_Keys (P) + 1);
               end loop;
               Get_Key (P, Number_Of_Keys (P), Sib);
            end;
            Do_Delete (Left, Sib);
            Set_Key (Page, Index, Sib);
         elsif Number_Of_Keys (Right) > Minimum_Keys (Right) then
            Trace ("Case 2b");
            declare
               P : Btree_Page_Handle := Right;
            begin
               while not Is_Leaf (P) loop
                  P := Get_Child (P, 1);
               end loop;
               Get_Key (P, 1, Sib);
            end;
            Do_Delete (Right, Sib);
            Set_Key (Page, Index, Sib);
         else
            Trace ("Case 2c");
            if not Is_Leaf (Left) then
               Set_Parent (Get_Child (Left, Number_Of_Keys (Left) + 1),
                           Right);
               Page.Get_Key (Index, Temp_Key);
               Insert_Key (Right, Temp_Key,
                           Get_Child (Left, Number_Of_Keys (Left) + 1));
            end if;

            Delete_Key (Page, Index);

            for I in 1 .. Number_Of_Keys (Left) loop
               if not Is_Leaf (Left) then
                  Set_Parent (Get_Child (Left, I), Right);
                  Left.Get_Key (I, Temp_Key);
                  Insert_Key (Right, Temp_Key,
                              Get_Child (Left, I));
               else
                  Left.Get_Key (I, Temp_Key);
                  Insert_Key (Right, Temp_Key);
               end if;
            end loop;

            Marlowe.File_Handles.Deallocate (Handle.File,
                                             Get_Location (Left));

            if not Is_Leaf (Right) then
               Do_Delete (Right, K);
            end if;
         end if;
      end Delete_Internal;

      ---------------
      -- Do_Delete --
      ---------------

      procedure Do_Delete (Page : in Btree_Page_Handle;
                           K    : in System.Storage_Elements.Storage_Array)
      is
      begin
         if Is_Leaf (Page) then
            Trace ("Deleting from leaf " & Image (Page.Get_Location));
            Delete_Key (Page, Find_Key (Page, K, True));
            Trace ("Finished deleting from leaf: " &
                     Image (Page.Get_Location));
         else
            declare
               Index : constant Slot_Index :=
                 Find_Key (Page, K, True);
               Index_Less_Than_Key_Count : constant Boolean :=
                 Index <= Number_Of_Keys (Page);
               Temp_Key : System.Storage_Elements.Storage_Array (K'Range);
            begin

               if Index_Less_Than_Key_Count then
                  Page.Get_Key (Index, Temp_Key);
               end if;

               if Index_Less_Than_Key_Count and then
                 Marlowe.Key_Storage.Equal (Temp_Key, K)
               then
                  Trace ("deleting internally");
                  Delete_Internal (Page, Index, K);
               else
                  Trace ("deleting from subtree");
                  Delete_From_Subtree (Page, Index, K);
               end if;
            end;
         end if;
      end Do_Delete;

      Root : Btree_Page_Handle;

   begin

      --  naive locking ...

      --  IMPORTANT
      --  The lack of locking in the rest of the Delete routine
      --  is because we have taken an exclusive locks on the
      --  btree header and root pages
      --

      Trace ("Locking header " & Image (Handle.Header.Get_Location));

      Btree_Header_Page_Handles.Exclusive_Lock (Handle.Header);

      Marlowe.File_Handles.Read
        (Handle.File,
         Page_Handles.Page_Handle'Class (Root),
         Marlowe.Btree_Header_Page_Handles.Get_Btree_Root
           (Handle.Header,
            Reference.Key_Index));
      Trace ("Locking root " & Image (Root.Get_Location));

      Exclusive_Lock (Root);
      Do_Delete (Root, Key);

      Trace ("Unlocking root " & Image (Root.Get_Location));

      Unlock (Root);

      Trace ("Unlocking header " & Image (Handle.Header.Get_Location));
      Btree_Header_Page_Handles.Unlock (Handle.Header);

      Trace ("delete finished");

   end Delete;

   -------------------
   -- Delete_Record --
   -------------------

   procedure Delete_Record (Handle   : in Btree_Handle;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index)
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Data : constant Data_Definition_Handle :=
               Get_Data_Definition (Handle, Table);
   begin

      declare
         use Marlowe.Tables;
         Addr : constant File_Page_And_Slot :=
                  Get_Record_Address (Handle, Table, Db_Index);
         Header : Marlowe.Tables.Record_Header;
         Data_Handle      : Marlowe.Handles.Data_Page_Handle.Table_Page_Handle;
      begin
         Data.Derive (Data_Handle);

         Data_Handle.Set_Page (Get_File_And_Page (Addr));
         Data_Handle.Exclusive_Lock;
         Data_Handle.Get_Table_Value (Get_Slot (Addr), Header);
         Header.Overflow := File_Page_And_Slot'Last;
         Data_Handle.Set_Table_Value (Get_Slot (Addr), Header);
         Data_Handle.Unlock;
      end;

      Data.Shared_Unlock;

   end Delete_Record;

   --------------------
   -- Deleted_Record --
   --------------------

   function Deleted_Record (Handle   : in Btree_Handle;
                            Table    : in Table_Index;
                            Db_Index : in Database_Index)
                           return Boolean
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Data : constant Data_Definition_Handle :=
               Get_Data_Definition (Handle, Table);
      Result : Boolean;
   begin

      declare
         use Marlowe.Tables;
         Addr : constant File_Page_And_Slot :=
                  Get_Record_Address (Handle, Table, Db_Index);
         Header : Marlowe.Tables.Record_Header;
         Data_Handle      : Marlowe.Handles.Data_Page_Handle.Table_Page_Handle;
      begin
         Data.Derive (Data_Handle);

         Data_Handle.Set_Page (Get_File_And_Page (Addr));
         Data_Handle.Shared_Lock;
         Data_Handle.Get_Table_Value (Get_Slot (Addr), Header);
         Result := Header.Overflow = File_Page_And_Slot'Last;
         Data_Handle.Shared_Unlock;
      end;

      Data.Shared_Unlock;

      return Result;

   end Deleted_Record;

   ---------------
   -- Dump_Tree --
   ---------------

   procedure Dump_Tree (Handle    : Btree_Handle;
                        Reference : Btree_Reference;
                        To_Output : Boolean          := False)
   is

      use Btree_Page_Handles;

      procedure Dump_Tree (Top : Btree_Page_Handle);
      --  Dump the tree, starting from the given page

      ---------------
      -- Dump_Tree --
      ---------------

      procedure Dump_Tree (Top : Btree_Page_Handle) is
      begin
         Dump_Page (Top, To_Output);
         if not Is_Leaf (Top) then
            for I in 1 .. Number_Of_Keys (Top) + 1 loop
               Dump_Tree (Get_Child (Top, I));
            end loop;
         end if;
      end Dump_Tree;

      Root : Btree_Page_Handle;

   begin

      Marlowe.File_Handles.Read
        (Handle.File,
         Page_Handles.Page_Handle'Class (Root),
         Marlowe.Btree_Header_Page_Handles.Get_Btree_Root
           (Handle.Header,
            Reference.Key_Index));
      Dump_Tree (Root);

   end Dump_Tree;

   ----------------
   -- Empty_Mark --
   ----------------

   function Empty_Mark
     (Key_Length : System.Storage_Elements.Storage_Count)
      return Btree_Mark
   is
      Result : Btree_Mark (Key_Length);
   begin
      Result.Slot := 0;
      return Result;
   end Empty_Mark;

   ------------
   -- Exists --
   ------------

   function Exists (Handle    : Btree_Handle;
                    Reference : Btree_Reference;
                    Key       : System.Storage_Elements.Storage_Array)
                   return Boolean
   is
   begin
      return Valid (Search (Handle, Reference, Key, Key, Closed, Closed,
                            Forward));
   end Exists;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Mark : in out Btree_Mark) is
   begin
      if Mark.Slot /= 0 then
--           Ada.Text_IO.Put_Line ("Finalizing mark: " &
--                                 Image (Mark.Page.Get_Location));
         Btree_Page_Handles.Shared_Unlock (Mark.Page);
         Mark.Slot := 0;
      end if;
   end Finalize;

   --------------------------
   -- Get_Cache_Statistics --
   --------------------------

   procedure Get_Cache_Statistics (Handle : in      Btree_Handle;
                                   Blocks :     out Natural;
                                   Pages  :     out Natural;
                                   Hits   :     out Natural;
                                   Misses :     out Natural)
   is
   begin
      File_Handles.Get_Cache_Statistics (Handle.File,
                                         Blocks, Pages, Hits, Misses);
   end Get_Cache_Statistics;

   -------------------------
   -- Get_Data_Definition --
   -------------------------

   function Get_Data_Definition
     (Handle : Btree_Handle;
      Index  : Table_Index)
      return Marlowe.Btree.Data_Definition_Handles.Data_Definition_Handle
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Data : Data_Definition_Handle := Handle.Table_Root;
      Local : Table_Index := Index;
      Max   : constant Table_Index :=
                Table_Index (Maximum_Data_Record_Count);
   begin
      Data.Shared_Lock;
      while Local > Max loop
         Local := Local - Max;
         declare
            Overflow : constant Data_Definition_Handle :=
                         Data.Get_Overflow_Page;
         begin
            Overflow.Shared_Lock;
            Data.Shared_Unlock;
            Data := Overflow;
         end;
      end loop;
      return Data;
   end Get_Data_Definition;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (From : Btree_Mark)
                    return System.Storage_Elements.Storage_Array
   is
      Result : System.Storage_Elements.Storage_Array
        (1 .. From.Key_Length);
   begin
      pragma Assert (Valid (From));
      Marlowe.Btree_Page_Handles.Get_Key (From.Page, From.Slot, Result);
      return Result;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   procedure Get_Key (From : in     Btree_Mark;
                      Key  :    out System.Storage_Elements.Storage_Array)
   is
   begin
      pragma Assert (Valid (From));
      Marlowe.Btree_Page_Handles.Get_Key (From.Page, From.Slot, Key);
   end Get_Key;

   --------------------
   -- Get_Key_Length --
   --------------------

   function Get_Key_Length (Reference : Btree_Reference)
                           return System.Storage_Elements.Storage_Offset
   is
   begin
      return Reference.Key_Length;
   end Get_Key_Length;

   -------------------------
   -- Get_Key_Table_Index --
   -------------------------

   function Get_Key_Table_Index (Reference : Btree_Reference)
                                 return Table_Index
   is
   begin
      return Reference.Table;
   end Get_Key_Table_Index;

   ----------------
   -- Get_Record --
   ----------------

   procedure Get_Record (Handle   : in Btree_Handle;
                         Index    : in Table_Index;
                         Db_Index : in Database_Index;
                         Data     : in System.Address)
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Table_Data : constant Data_Definition_Handle :=
                     Get_Data_Definition (Handle, Index);
   begin

      Trace ("Get_Record:" & Index'Img & Db_Index'Img);

      declare
         use Marlowe.Tables;
         Addr : constant File_Page_And_Slot :=
                  Get_Record_Address (Handle, Index, Db_Index);
         Header : Marlowe.Tables.Record_Header;
         Length : constant Storage_Count :=
                    Table_Data.Get_Record_Length (Index);
         Have_Overflow : constant Boolean :=
                           Length > Direct_Record_Storage_Units;
         Result        : Storage_Array (1 .. Length);
         for Result'Address use Data;
         Data_Handle      : Marlowe.Handles.Data_Page_Handle.Table_Page_Handle;
         Overflow_Handle  :
         Marlowe.Handles.Storage_Element_Page_Handle.Table_Page_Handle;
         Last_Header      : Storage_Count := Length;
      begin

         if Last_Header > Direct_Record_Storage_Units then
            Last_Header := Direct_Record_Storage_Units;
         end if;

         Table_Data.Derive (Data_Handle);

         Data_Handle.Set_Page (Get_File_And_Page (Addr));
         Data_Handle.Shared_Lock;
         Data_Handle.Get_Table_Value (Get_Slot (Addr), Header);
         Result (1 .. Last_Header) := Header.Data (1 .. Last_Header);
         if Have_Overflow then
            Data_Handle.Derive (Overflow_Handle);
            Overflow_Handle.Set_Page (Get_File_And_Page (Header.Overflow));
            Overflow_Handle.Shared_Lock;

            declare
               Overflow_Slot : constant Slot_Index :=
                                 Get_Slot (Header.Overflow);
            begin
               for I in Last_Header + 1 .. Result'Last loop
                  Result (I) :=
                    Overflow_Handle.Get_Table_Value
                      (Slot_Index (I - Last_Header - 1) + Overflow_Slot);
               end loop;
            end;

            Overflow_Handle.Shared_Unlock;
         end if;
         Data_Handle.Shared_Unlock;
      end;

      Table_Data.Shared_Unlock;

   end Get_Record;

   ------------------------
   -- Get_Record_Address --
   ------------------------

   function Get_Record_Address (Handle   : in Btree_Handle;
                                Index    : in Table_Index;
                                Db_Index : in Database_Index)
                               return File_Page_And_Slot
   is
      use Marlowe.Handles.Page_Pointer_Page_Handle;
      use Marlowe.Btree.Data_Definition_Handles;
      Data : constant Data_Definition_Handle :=
               Get_Data_Definition (Handle, Index);
      Record_Root_Location : constant File_And_Page :=
                               Data.Get_Record_Root (Index);
      Reference            : constant Table_Reference :=
        Handle.Table_Refs.Element (Positive (Index));
      Page_Pointers_Per_Page : constant Database_Index :=
        Database_Index (Reference.Page_Pointers_Per_Page);
      Db_Records_Per_Page : constant Database_Index :=
        Database_Index (Reference.Db_Records_Per_Page);

      Slots : constant Array_Of_Slots :=
                Get_Slot_Path (Page_Pointers_Per_Page,
                               Db_Records_Per_Page,
                               Db_Index);

      Record_Current       : Table_Page_Handle;
      Record_Next          : Table_Page_Handle;

      Page          : File_And_Page    := Record_Root_Location;

   begin

      Data.Derive (Record_Current);
      Record_Current.Set_Page (Record_Root_Location);
      Record_Current.Shared_Lock;

      for I in Slots'Range loop
         Page := Record_Current.Get_Table_Value (Slots (I));
         exit when I = Slots'Last - 1;
         Record_Current.Derive (Record_Next);
         Record_Next.Set_Page (Page);
         Record_Next.Shared_Lock;
         Record_Current.Shared_Unlock;
         Record_Current := Record_Next;
      end loop;

      Record_Current.Shared_Unlock;
      Data.Shared_Unlock;

      return To_File_Page_And_Slot (Page, Slots (Slots'Last));

   end Get_Record_Address;

   -----------------------
   -- Get_Record_Length --
   -----------------------

--     function Get_Record_Length (Handle    : in Btree_Handle;
--                                 Index     : in Table_Index)
--                                 return Storage_Count
--     is
--     begin
--        return Handle.Tables.Get_Record_Length (Index);
--     end Get_Record_Length;

   ---------------------
   -- Get_Record_Name --
   ---------------------

   function Get_Record_Name (Handle    : in Btree_Handle;
                             Index     : in Table_Index)
                             return String
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Table_Data : constant Data_Definition_Handle :=
                     Get_Data_Definition (Handle, Index);
      Result     : constant String :=
                     Table_Data.Get_Record_Name (Index);
   begin
      Table_Data.Shared_Unlock;
      return Result;
   end Get_Record_Name;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference (Handle    : in     Btree_Handle;
                           Name      : in     String)
                          return Btree_Reference
   is
      use List_Of_References;
      It : Cursor := First (Handle.Refs);
   begin
      while Has_Element (It) loop
         if Ada.Strings.Unbounded.To_String (Element (It).Name) = Name then
            return Element (It);
         end if;
         Next (It);
      end loop;
      raise Ada.IO_Exceptions.Name_Error;
   end Get_Reference;

   --------------
   -- Get_Root --
   --------------

   function Get_Root (Handle    : in Btree_Handle;
                      Reference : in Btree_Reference)
                     return Marlowe.Btree_Page_Handles.Btree_Page_Handle
   is
      Root : Marlowe.Btree_Page_Handles.Btree_Page_Handle;
   begin
      Marlowe.File_Handles.Read
        (Handle.File,
         Page_Handles.Page_Handle'Class (Root),
         Marlowe.Btree_Header_Page_Handles.Get_Btree_Root
           (Handle.Header,
            Reference.Key_Index));
      return Root;
   end Get_Root;

   -------------------
   -- Get_Slot_Path --
   -------------------

   function Get_Slot_Path (Page_Pointers_Per_Page : Database_Index;
                           Db_Records_Per_Page    : Database_Index;
                           Db_Index               : Database_Index)
                           return Array_Of_Slots
   is
      Buffer : Array_Of_Slots (1 .. Max_Root_Pages);
      Acc    : Database_Index := Db_Index - 1;
      Count  : Positive       := 1;
   begin

      Buffer (Count) := Slot_Index (Acc mod Db_Records_Per_Page) + 1;
      Acc := Acc / Db_Records_Per_Page;

      while Acc > 0 loop
         Count := Count + 1;
         Buffer (Count) := Slot_Index (Acc mod Page_Pointers_Per_Page) + 1;
         Acc := Acc / Page_Pointers_Per_Page;
      end loop;

      Count := Count + 1;
      Buffer (Count) := Slot_Index (Count - 1);

      return R       : Array_Of_Slots (1 .. Count) do
         for J in R'Range loop
            R (J) := Buffer (Count + 1 - J);
         end loop;
      end return;

   end Get_Slot_Path;

   ------------
   -- Insert --
   ------------

   procedure Insert (Handle     : in Btree_Handle;
                     Reference  : in Btree_Reference;
                     Key        : in System.Storage_Elements.Storage_Array)
   is
      use Marlowe.Btree_Page_Handles;

      procedure Split_Child (Child  : in out Btree_Page_Handle;
                             Index  : in     Slot_Index;
                             Parent : in     Btree_Page_Handle);

      procedure Insert_Nonfull (Page : in Btree_Page_Handle);

      --------------------
      -- Insert_Nonfull --
      --------------------

      procedure Insert_Nonfull (Page : in Btree_Page_Handle) is
      begin
         if Is_Leaf (Page) then
            if Update_Locked (Page) then
               Upgrade_To_Exclusive (Page);
            end if;
            Insert_Key (Page, Key);
            Unlock (Page);
         else
            declare
               Index : constant Slot_Index :=
                 Find_Key (Page, Key, True);
               Child : Btree_Page_Handle := Get_Child (Page, Index);
            begin
               Update_Lock (Child);
               if Number_Of_Keys (Child) = Maximum_Keys (Child) then
                  Upgrade_To_Exclusive (Page);
                  Upgrade_To_Exclusive (Child);
                  Split_Child (Child, Index, Page);
               end if;
               Unlock (Page);
               Insert_Nonfull (Child);
            end;
         end if;
      end Insert_Nonfull;

      -----------------
      -- Split_Child --
      -----------------

      procedure Split_Child (Child  : in out Btree_Page_Handle;
                             Index  : in     Slot_Index;
                             Parent : in     Btree_Page_Handle)
      is
         New_Child : Btree_Page_Handle;
      begin

         Marlowe.File_Handles.Allocate
           (Handle.File,
            Page_Handles.Page_Handle'Class (New_Child));

         Marlowe.Btree_Page_Handles.Set_Page_Info
           (New_Child, Is_Leaf (Child), Get_Key_Length (Child));

         Split (Child, New_Child, Index, Parent);

         declare
            use Marlowe.Key_Storage;
            Page_Key : System.Storage_Elements.Storage_Array (Key'Range);
         begin
            Get_Key (Parent, Index, Page_Key);
            if Index <= Number_Of_Keys (Parent)
              and then Key_Storage.Compare (Page_Key, Key) = Less
            then
               Unlock (Child);
               Child := New_Child;
            else
               Unlock (New_Child);
            end if;
         end;

--           if not Check_Tree (Handle, Reference) then
--              Ada.Text_IO.Put_Line ("Tree not ok after Split");
--              Dump_Tree (Handle, Reference);
--              raise Constraint_Error;
--           end if;

      end Split_Child;

      Root : Btree_Page_Handle;
   begin

      Btree_Header_Page_Handles.Exclusive_Lock (Handle.Header);

      Marlowe.File_Handles.Read
        (Handle.File,
         Page_Handles.Page_Handle'Class (Root),
         Marlowe.Btree_Header_Page_Handles.Get_Btree_Root
           (Handle.Header,
            Reference.Key_Index));

      Update_Lock (Root);

      if Number_Of_Keys (Root) = Maximum_Keys (Root) then

         Upgrade_To_Exclusive (Root);

         declare
            New_Root : Btree_Page_Handle;
         begin
            Marlowe.File_Handles.Allocate
              (Handle.File,
               Page_Handles.Page_Handle'Class (New_Root));

            Marlowe.Btree_Page_Handles.Set_Page_Info
              (New_Root, False, Get_Key_Length (Root));

            Btree_Header_Page_Handles.Set_Btree_Root
              (Handle.Header, Reference.Key_Index, Get_Location (New_Root));

            Btree_Header_Page_Handles.Unlock (Handle.Header);

            Set_Child (New_Root, 1, Root);
            Set_Parent (Root, New_Root);

            Split_Child (Root, 1, New_Root);
            Unlock (Root);
            Insert_Nonfull (New_Root);
         end;
      else
         Btree_Header_Page_Handles.Unlock (Handle.Header);
         Insert_Nonfull (Root);
      end if;

   end Insert;

   -------------------
   -- Insert_Record --
   -------------------

   function Insert_Record
     (Handle    : in Btree_Handle;
      Table     : in Table_Index)
      return Database_Index
   is
      use Marlowe.Handles.Page_Pointer_Page_Handle;
      use Marlowe.Btree.Data_Definition_Handles;
      Data : constant Data_Definition_Handle :=
               Get_Data_Definition (Handle, Table);
      Record_Root_Location : constant File_And_Page :=
                               Data.Get_Record_Root (Table);
      Data_Handle          : Handles.Data_Page_Handle.Table_Page_Handle;
      Overflow_Handle      :
      Handles.Storage_Element_Page_Handle.Table_Page_Handle;
      Db_Index             : Database_Index;
      Reference            : constant Table_Reference :=
        Handle.Table_Refs.Element (Positive (Table));
      Length               : Storage_Count;
      Page_Pointers_Per_Page : constant Database_Index :=
        Database_Index (Reference.Page_Pointers_Per_Page);
      Db_Records_Per_Page : constant Database_Index :=
                                 Database_Index
                                   (Reference.Db_Records_Per_Page);
      Header        : Marlowe.Tables.Record_Header;

      Record_Current       : Table_Page_Handle;
      Record_Next          : Table_Page_Handle;

      Page          : File_And_Page    := Record_Root_Location;
      Slot          : Slot_Index;

      procedure Ensure_Page;
      procedure Ensure_Data_Page;

      ----------------------
      -- Ensure_Data_Page --
      ----------------------

      procedure Ensure_Data_Page is
      begin
         if Page = 0 then
            Marlowe.File_Handles.Allocate
              (Handle.File,
               Page_Handles.Page_Handle'Class (Data_Handle));
            Page := Data_Handle.Get_Location;
            Record_Current.Set_Table_Value (Slot, Page);
         else
            Record_Current.Derive (Data_Handle);
            Data_Handle.Set_Page (Page);
            Data_Handle.Exclusive_Lock;
         end if;
      end Ensure_Data_Page;

      -----------------
      -- Ensure_Page --
      -----------------

      procedure Ensure_Page is
      begin
         if Page = 0 then
            Marlowe.File_Handles.Allocate
              (Handle.File,
               Page_Handles.Page_Handle'Class (Record_Next));
            Page := Record_Next.Get_Location;
            Record_Current.Set_Table_Value (Slot, Page);
         else
            Record_Current.Derive (Record_Next);
            Record_Next.Set_Page (Page);
            Record_Next.Exclusive_Lock;
         end if;
         Record_Current.Unlock;
         Record_Current := Record_Next;
      end Ensure_Page;

   begin

      Data.Shared_Unlock;
      Data.Exclusive_Lock;

      Db_Index := Data.Allocate_Record (Table);
      Length   := Data.Get_Record_Length (Table);

      Header.Data := (others => 0);
      Header.Overflow := 0;

      if Length > Marlowe.Tables.Direct_Record_Storage_Units then
         --  We're going to allocate an overflow page

         declare
            Overflow      : File_Page_And_Slot :=
              Data.Get_Next_Record_Overflow (Table);
            Excess        : constant Slot_Index :=
              Slot_Index (Length) -
              Marlowe.Tables.Direct_Record_Storage_Units;
            Overflow_Slot : Slot_Index;
         begin
            Overflow_Slot := Get_Slot (Overflow);

            if Overflow = Null_File_Page_And_Slot or else
              Overflow_Slot + Excess >
              Slot_Index (Marlowe.Pages.Elements_Per_Page (1))
            then
               Overflow_Slot := 1;
               Marlowe.File_Handles.Allocate
                 (Handle.File,
                  Page_Handles.Page_Handle'Class (Overflow_Handle));
               Overflow :=
                 To_File_Page_And_Slot (Overflow_Handle.Get_Location,
                                        Overflow_Slot);
               Overflow_Handle.Unlock;
            end if;

            Header.Overflow := Overflow;

            Overflow :=
              To_File_Page_And_Slot
                (Get_File_And_Page (Overflow),
                 Overflow_Slot + Excess);

            Data.Set_Next_Record_Overflow
              (Table, Overflow);
         end;

      end if;

      Data.Unlock;

      Data.Derive (Record_Current);
      Record_Current.Set_Page (Record_Root_Location);
      Record_Current.Exclusive_Lock;

      Trace ("insert_record: db_index =" & Db_Index'Img);

      declare
         Slot_Path : constant Array_Of_Slots :=
                       Get_Slot_Path (Page_Pointers_Per_Page,
                                      Db_Records_Per_Page,
                                      Db_Index);
      begin
         Trace ("Slot_Path length: " & Natural'Image (Slot_Path'Length));
         for I in Slot_Path'Range loop
            Trace (Slot_Index'Image (Slot_Path (I)));
         end loop;

         for I in 1 .. Slot_Path'Last - 1 loop
            Slot := Slot_Path (I);
            Page := Record_Current.Get_Table_Value (Slot);
            if I < Slot_Path'Last - 1 then
               Ensure_Page;
            else
               Ensure_Data_Page;
            end if;

         end loop;

         Slot := Slot_Path (Slot_Path'Last);

      end;

      Data_Handle.Set_Table_Value (Slot, Header);

      Record_Current.Unlock;
      Data_Handle.Unlock;

      return Db_Index;

   end Insert_Record;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Handle   : in Btree_Handle;
                        Table    : in Table_Index)
                        return Database_Index
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Table_Data : constant Data_Definition_Handle :=
                     Get_Data_Definition (Handle, Table);
      Last       : constant Database_Index :=
                     Table_Data.Last_Record_Index (Table);
   begin
      Table_Data.Shared_Unlock;
      return Last;
   end Last_Index;

   ----------
   -- Next --
   ----------

   procedure Next (Current : in out Btree_Mark) is

      use Marlowe.Btree_Page_Handles;

      function Next_OK return Boolean;

      -------------
      -- Next_OK --
      -------------

      function Next_OK return Boolean is
      begin
         return (Current.Scan = Forward and then
                 Current.Slot < Number_Of_Keys (Current.Page))
           or else (Current.Scan = Backward and then Current.Slot > 1)
           or else (not Is_Leaf (Current.Page)
                    and then Current.Scan = Forward
                    and then Current.Slot <= Number_Of_Keys (Current.Page));
      end Next_OK;

      Current_Key : System.Storage_Elements.Storage_Array
        (1 .. Current.Key_Length);
      Success     : Boolean := False;

   begin

      Get_Key (Current.Page, Current.Slot, Current_Key);

      if Is_Leaf (Current.Page) then

         if Next_OK then
            if Current.Scan = Forward then
               Current.Slot := Current.Slot + 1;
            else
               Current.Slot := Current.Slot - 1;
            end if;
            Success := True;
         else
            while Get_Parent (Current.Page) /= 0 loop
               declare
                  Parent : Btree_Page_Handle;
                  Index  : Slot_Index;
               begin
                  Parent := Get_Parent (Current.Page);
                  Shared_Lock (Parent);
                  Shared_Unlock (Current.Page);

                  Index :=
                    Find_Key (Parent,
                              Current_Key,
                              Current.Scan = Forward);

                  if Current.Scan = Forward then
                     if Index <= Number_Of_Keys (Parent) then
                        Current.Page := Parent;
                        Current.Slot := Index;
                        Success      := True;
                        exit;
                     else
                        Current.Page := Parent;
                     end if;
                  else
                     if Index > 1 then
                        Current.Page := Parent;
                        Current.Slot := Index - 1;
                        Success      := True;
                        exit;
                     else
                        Current.Page := Parent;
                     end if;
                  end if;
               end;
            end loop;
         end if;
      else
         if Current.Scan = Backward then
            declare
               Child : Btree_Page_Handle :=
                 Get_Child (Current.Page, Current.Slot);
            begin
               Shared_Lock (Child);
               Shared_Unlock (Current.Page);
               while not Is_Leaf (Child) loop
                  declare
                     New_Child : constant Btree_Page_Handle :=
                       Get_Child (Child, Number_Of_Keys (Child) + 1);
                  begin
                     Shared_Lock (New_Child);
                     Shared_Unlock (Child);
                     Child := New_Child;
                  end;
               end loop;
               Current.Page := Child;
               Current.Slot := Number_Of_Keys (Child);
            end;
            Success := True;
         elsif Next_OK then
            declare
               Child : Btree_Page_Handle :=
                 Get_Child (Current.Page, Current.Slot + 1);
            begin
               Shared_Lock (Child);
               Shared_Unlock (Current.Page);
               while not Is_Leaf (Child) loop
                  declare
                     New_Child : constant Btree_Page_Handle :=
                       Get_Child (Child, 1);
                  begin
                     Shared_Lock (New_Child);
                     Shared_Unlock (Child);
                     Child := New_Child;
                  end;
               end loop;
               Current.Page := Child;
               Current.Slot := 1;
            end;
            Success := True;
         else
            while Get_Parent (Current.Page) /= 0 loop
               declare
                  Parent : constant Btree_Page_Handle :=
                    Get_Parent (Current.Page);
                  Index  : Slot_Index;
               begin
                  Shared_Lock (Parent);
                  Shared_Unlock (Current.Page);
                  Index :=
                    Find_Key (Parent,
                              Current_Key,
                              Current.Scan = Forward);
                  if Index <= Number_Of_Keys (Parent) then
                     Current.Page := Parent;
                     Current.Slot := Index;
                     Success      := True;
                     exit;
                  else
                     Current.Page := Parent;
                  end if;
               end;
            end loop;
         end if;

      end if;

      if not Success or else not Consistent (Current) then
         Shared_Unlock (Current.Page);
         Current.Slot := 0;
      end if;

   end Next;

   ----------
   -- Open --
   ----------

   procedure Open (Handle : in out Btree_Handle;
                   Name   : in     String;
                   Magic  : in     Marlowe_Magic_Number)
   is
   begin
      Handle := new Btree_Handle_Record;

      if Marlowe.Allocation.Debug_Allocation then
         Marlowe.Allocation.Allocate (Handle.all'Size, "btree-handle");
      end if;

      Marlowe.File_Handles.Open (Handle.File,
                                 Marlowe.Configuration.Working_Directory &
                                 Name);
      Marlowe.File_Handles.Read
        (Handle.File,
         Page_Handles.Page_Handle'Class (Handle.Header),
         Marlowe.File_Handles.First_User_Page (Handle.File));

      if Handle.Header.Get_Magic /= Magic then
         Marlowe.File_Handles.Close (Handle.File);
         raise Bad_Magic_Number;
      end if;

      Marlowe.File_Handles.Read
        (Handle.File,
         Page_Handles.Page_Handle'Class (Handle.Table_Root),
         Handle.Header.Get_Table_Page);

      declare
         use Marlowe.Btree.Data_Definition_Handles;
         Table_Data  : Data_Definition_Handle := Handle.Table_Root;
         First_Index : Table_Index := 0;
      begin
         loop
            for I in 1 .. Table_Index (Table_Data.Number_Of_Data_Records)
            loop
               declare
                  Index  : constant Table_Index := First_Index + I;
                  Loc    : constant File_And_Page :=
                             Table_Data.Get_Record_Root (Index);
                  Length : constant Storage_Count :=
                             Table_Data.Get_Record_Length (Index);
                  Ref    : constant Table_Reference :=
                             Create_Table_Reference (Index, Length, Loc);
               begin
                  Handle.Table_Refs.Append (Ref);
               end;
            end loop;

            exit when Table_Data.Get_Overflow_Page = 0;

            Table_Data := Table_Data.Get_Overflow_Page;
            First_Index :=
              First_Index + Table_Index (Maximum_Data_Record_Count);
         end loop;
      end;

      declare
         use Marlowe.Btree_Header_Page_Handles;
         Ref : Btree_Reference;
         Header : Btree_Header_Page_Handles.Btree_Header_Page_Handle :=
           Handle.Header;
         Index  : Natural := 0;
      begin
         for I in 1 .. Get_Total_Btrees (Handle.Header) loop

            Index := Index + 1;
            if Index > Btree_Header_Page_Handles.Maximum_Btree_Count then
               Index := 1;
               Header := Btree_Header_Page_Handles.Get_Overflow_Page (Header);
            end if;

            Ref.Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (Get_Btree_Name (Header, Index));
            Ref.Key_Index := I;
            Ref.Key_Length :=
              System.Storage_Elements.Storage_Offset
              (Btree_Header_Page_Handles.Get_Btree_Key_Length (Header,
                 Index));
            List_Of_References.Append (Handle.Refs, Ref);
         end loop;
      end;
   end Open;

   --------------------------
   -- Read_Field_Extension --
   --------------------------

   function Read_Field_Extension
     (Handle    : Btree_Handle;
      Reference : File_And_Page)
      return System.Storage_Elements.Storage_Array
   is
      use Marlowe.Handles.Field_Extension_Page_Handles;

      function Read_Rest (Start : File_And_Page) return Storage_Array;

      ---------------
      -- Read_Rest --
      ---------------

      function Read_Rest (Start : File_And_Page) return Storage_Array is
         Rest_Handle : Field_Extension_Page_Handle;
      begin
         Handle.Header.Derive (Rest_Handle);
         Rest_Handle.Set_Page (Start);
         Rest_Handle.Shared_Lock;
         if Rest_Handle.Overflow_Page = 0 then
            return Rest_Handle.Extension_Data;
         else
            return Rest_Handle.Extension_Data
              & Read_Rest (Rest_Handle.Overflow_Page);
         end if;
      end Read_Rest;

   begin
      return Read_Rest (Reference);
   end Read_Field_Extension;

   -------------------
   -- Record_Length --
   -------------------

   function Record_Length (Handle : in Btree_Handle;
                           Table  : in Table_Index)
                           return System.Storage_Elements.Storage_Count
   is
      use Marlowe.Btree.Data_Definition_Handles;
      Table_Data : constant Data_Definition_Handle :=
                     Get_Data_Definition (Handle, Table);
   begin
      return Table_Data.Get_Record_Length (Table);
   end Record_Length;

   -------------
   -- Release --
   -------------

   procedure Release (Mark : in out Btree_Mark) is
   begin
      if Mark.Slot /= 0 then
         Btree_Page_Handles.Shared_Unlock (Mark.Page);
         Mark.Slot := 0;
      end if;
   end Release;

   ------------
   -- Search --
   ------------

   function Search
     (Handle          : Btree_Handle;
      Reference       : Btree_Reference;
      Scan            : Scan_Type)
     return Btree_Mark
   is
      Start_Key, Finish_Key :
        System.Storage_Elements.Storage_Array (1 .. Reference.Key_Length);
   begin
      Start_Key := (others => 0);
      Finish_Key := (others => 255);
      return Search (Handle, Reference, Start_Key, Finish_Key,
                     Closed, Closed, Scan);
   end Search;

   ------------
   -- Search --
   ------------

   function Search
     (Handle          : Btree_Handle;
      Reference       : Btree_Reference;
      Start           : System.Storage_Elements.Storage_Array;
      Finish          : System.Storage_Elements.Storage_Array;
      Start_Interval  : Interval_Type;
      Finish_Interval : Interval_Type;
      Scan            : Scan_Type)
     return Btree_Mark
   is

      use Marlowe.Btree_Page_Handles;

      function First_Key return System.Storage_Elements.Storage_Array;
      --  Return either Start or Finish, depending on the
      --  value of Scan

      function First_Interval return Interval_Type;
      --  Return either Start_Interval or Finish_Interval,
      --  depending on the value of Scan

      function Perform_Search
        (Page : Btree_Page_Handle)
         return Btree_Mark;
      --  Perform the search on the given Page, and recurse to
      --  child pages as necessary

      --------------------
      -- First_Interval --
      --------------------

      function First_Interval return Interval_Type is
      begin
         if Scan = Forward then
            return Start_Interval;
         else
            return Finish_Interval;
         end if;
      end First_Interval;

      ---------------
      -- First_Key --
      ---------------

      function First_Key return System.Storage_Elements.Storage_Array is
      begin
         if Scan = Forward then
            return Start;
         else
            return Finish;
         end if;
      end First_Key;

      Key : constant System.Storage_Elements.Storage_Array := First_Key;
      Interval : constant Interval_Type := First_Interval;

      --------------------
      -- Perform_Search --
      --------------------

      function Perform_Search (Page : in Btree_Page_Handle)
                         return Btree_Mark
      is
         use Marlowe.Key_Storage;
         Index  : constant Slot_Index :=
           Find_Key (Page, Key, Scan = Forward);
         Page_Key : System.Storage_Elements.Storage_Array (Start'Range);
         Result : Btree_Mark (System.Storage_Elements.Storage_Count
                              (Get_Key_Length (Page)));
      begin

--           Trace ("Do_Search: " & Image (Key) &
--                              " on page " & Image (Get_Location (Page)));

         if Index <= Number_Of_Keys (Page) then

            Get_Key (Page, Index, Page_Key);

            if Equal (Page_Key, Key) then
               --  Exact match
               Result :=
                 (Ada.Finalization.Controlled with
                  System.Storage_Elements.Storage_Count
                  (Get_Key_Length (Page)),
                  Reference, Index, Page, Scan, Start, Finish,
                  Start_Interval, Finish_Interval);
               if Interval = Closed then
                  if not Consistent (Result) then
                     return Empty_Mark (Start'Length);
                  end if;
               else
                  Next (Result);
               end if;
               return Result;
            elsif Is_Leaf (Page) then
               declare
                  Result : Btree_Mark :=
                    (Ada.Finalization.Controlled with
                     System.Storage_Elements.Storage_Count
                     (Get_Key_Length (Page)),
                     Reference, Index, Page, Scan, Start, Finish,
                     Start_Interval, Finish_Interval);
               begin
                  if Scan = Backward then
                     Next (Result);
                  end if;

                  if not Consistent (Result) then
                     return Empty_Mark (Start'Length);
                  end if;
                  return Result;
               end;
            else
               declare
                  Child : constant Btree_Page_Handle :=
                            Get_Child (Page, Index);
               begin

                  Shared_Lock (Child);
                  Shared_Unlock (Page);
                  return Perform_Search (Child);
               end;
            end if;
         else
            if Is_Leaf (Page) then
               if Scan = Forward then
                  declare
                     P       : Btree_Page_Handle := Page;
                     Q       : Btree_Page_Handle;
                     P_Index : Slot_Index := Index;
                  begin

                     while not Is_Root (P) loop
                        Q       := Get_Parent (P);
                        Shared_Lock (Q);
                        Shared_Unlock (P);
                        P := Q;
                        P_Index :=
                          Find_Key (P, Key, Scan /= Backward);
                        exit when P_Index <= Number_Of_Keys (P);
                     end loop;

                     if P_Index <= Number_Of_Keys (P) then
                        declare
                           Result : constant Btree_Mark :=
                             (Ada.Finalization.Controlled with
                              System.Storage_Elements.Storage_Count
                              (Get_Key_Length (P)),
                              Reference, P_Index, P, Scan, Start, Finish,
                              Start_Interval, Finish_Interval);
                        begin
                           if not Consistent (Result) then
                              return Empty_Mark (Start'Length);
                           end if;
                           return Result;
                        end;
                     else
                        Shared_Unlock (P);
                        return Empty_Mark (Start'Length);
                     end if;
                  end;
               else
                  Shared_Unlock (Page);
                  return Empty_Mark (Start'Length);
               end if;
            else
               declare
                  Child : constant Btree_Page_Handle :=
                            Get_Child (Page, Index);
               begin
                  Shared_Lock (Child);
                  Shared_Unlock (Page);
                  return Perform_Search (Child);
               end;
            end if;
         end if;
      end Perform_Search;

      Root : Btree_Page_Handle;

   begin

      Btree_Header_Page_Handles.Shared_Lock (Handle.Header);

      Marlowe.File_Handles.Read
        (Handle.File,
         Page_Handles.Page_Handle'Class (Root),
         Marlowe.Btree_Header_Page_Handles.Get_Btree_Root
           (Handle.Header,
            Reference.Key_Index));

      Btree_Page_Handles.Shared_Lock (Root);
      Btree_Header_Page_Handles.Shared_Unlock (Handle.Header);

      return Perform_Search (Root);

   end Search;

   -----------
   -- Trace --
   -----------

   procedure Trace (Message : String) is
   begin
      if Marlowe.Trace.Tracing then
         Marlowe.Trace.Trace (Message);
      end if;
   end Trace;

   -----------
   -- Valid --
   -----------

   function Valid (Item : Btree_Mark) return Boolean is
   begin
      return Item.Slot /= 0;
   end Valid;

   -----------------
   -- Valid_Index --
   -----------------

   function Valid_Index (Handle   : in Btree_Handle;
                         Table    : in Table_Index;
                         Db_Index : in Database_Index)
                        return Boolean
   is
      Last : Database_Index;
   begin
      if Db_Index = 0 then
         return False;
      end if;

      declare
         use Marlowe.Btree.Data_Definition_Handles;
         Table_Data : constant Data_Definition_Handle :=
                        Get_Data_Definition (Handle, Table);
      begin
         Last := Table_Data.Last_Record_Index (Table);
         Table_Data.Shared_Unlock;
      end;

      return Db_Index <= Last;
   end Valid_Index;

   ---------------------------
   -- Write_Field_Extension --
   ---------------------------

   procedure Write_Field_Extension
     (Handle    : Btree_Handle;
      Reference : File_And_Page;
      Data      : System.Storage_Elements.Storage_Array)
   is
      use Marlowe.Handles.Field_Extension_Page_Handles;
      Extension_Handle  : Field_Extension_Page_Handle;
      Next              : Storage_Offset;
      Overflow_Location : File_And_Page := Reference;
   begin

      Handle.Header.Derive (Extension_Handle);
      Extension_Handle.Set_Page (Reference);
      Extension_Handle.Exclusive_Lock;
      Overflow_Location := Extension_Handle.Overflow_Page;

      Extension_Handle.Set_Extension_Data (Data, Next);
      while Next < Data'Last loop
         declare
            Next_Handle : Field_Extension_Page_Handle;
         begin
            Extension_Handle.Derive (Next_Handle);
            if Overflow_Location = 0 then
               Marlowe.File_Handles.Allocate
                 (File => Handle.File,
                  Page => Next_Handle);
               Extension_Handle.Set_Overflow_Page (Next_Handle.Get_Location);
            else
               Next_Handle.Set_Page (Overflow_Location);
               Next_Handle.Exclusive_Lock;
            end if;
            Extension_Handle.Unlock;

            Overflow_Location := Next_Handle.Overflow_Page;
            Next_Handle.Set_Extension_Data
              (Data (Next .. Data'Last), Next);
            Extension_Handle := Next_Handle;
         end;
      end loop;

      while Overflow_Location /= 0 loop
         declare
            Next_Handle : Field_Extension_Page_Handle;
            Next_Overflow : File_And_Page;
         begin
            Extension_Handle.Derive (Next_Handle);
            Next_Handle.Set_Page (Overflow_Location);
            Next_Overflow := Next_Handle.Overflow_Page;
            Next_Handle.Unlock;
            Marlowe.File_Handles.Deallocate (Handle.File, Overflow_Location);
            Overflow_Location := Next_Overflow;
         end;
      end loop;

      Extension_Handle.Unlock;

   end Write_Field_Extension;

   ------------------
   -- Write_Record --
   ------------------

   procedure Write_Record (Handle   : in Btree_Handle;
                           Index    : in Table_Index;
                           Db_Index : in Database_Index;
                           Data     : in System.Address)
   is
      use Marlowe.Tables;
      use Marlowe.Btree.Data_Definition_Handles;
      Table_Data       : constant Data_Definition_Handle :=
                           Get_Data_Definition (Handle, Index);
      Addr             : constant File_Page_And_Slot :=
                           Get_Record_Address (Handle, Index, Db_Index);
      Header           : Marlowe.Tables.Record_Header;
      Length           : constant Storage_Count :=
                           Table_Data.Get_Record_Length (Index);
      Have_Overflow    : constant Boolean :=
                           Length > Direct_Record_Storage_Units;
      Result           : Storage_Array (1 .. Length);
      for Result'Address use Data;
      Data_Handle      : Marlowe.Handles.Data_Page_Handle.Table_Page_Handle;
      Overflow_Handle  :
      Marlowe.Handles.Storage_Element_Page_Handle.Table_Page_Handle;
      Overflow_Page    : File_And_Page;
      Overflow_Slot    : Slot_Index;
      Last_Header      : Storage_Count := Length;
   begin
      if Last_Header > Direct_Record_Storage_Units then
         Last_Header := Direct_Record_Storage_Units;
      end if;

      Table_Data.Derive (Data_Handle);

      Data_Handle.Set_Page (Get_File_And_Page (Addr));
      Data_Handle.Exclusive_Lock;
      if Have_Overflow then
         Data_Handle.Get_Table_Value (Get_Slot (Addr), Header);
         Overflow_Page := Get_File_And_Page (Header.Overflow);
         Overflow_Slot := Get_Slot (Header.Overflow);
      end if;

      Header.Data (1 .. Last_Header) := Result (1 .. Last_Header);
      Data_Handle.Set_Table_Value (Get_Slot (Addr), Header);

      if Have_Overflow then
         Data_Handle.Derive (Overflow_Handle);
         Overflow_Handle.Set_Page (Overflow_Page);
         Overflow_Handle.Exclusive_Lock;

         for I in Last_Header + 1 .. Result'Last loop
            Overflow_Handle.Set_Table_Value
              (Slot_Index (I - Last_Header - 1) + Overflow_Slot,
               Result (I));
         end loop;

         Overflow_Handle.Unlock;
      end if;
      Data_Handle.Unlock;
      Table_Data.Shared_Unlock;

   end Write_Record;

end Marlowe.Btree_Handles;
