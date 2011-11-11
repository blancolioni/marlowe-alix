with Ada.Unchecked_Deallocation;

with Marlowe.Allocation;

package body Marlowe.Pages is

   -----------------------
   -- Elements_Per_Page --
   -----------------------

   function Elements_Per_Page
     (Element_Size : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      return Page_Contents_Units / Element_Size;
   end Elements_Per_Page;

   -----------------------
   -- Get_File_And_Page --
   -----------------------

   function Get_File_And_Page (From : Page) return File_And_Page is
   begin
      return Get_File_And_Page (From.all);
   end Get_File_And_Page;

   -----------------------
   -- Get_File_And_Page --
   -----------------------

   function Get_File_And_Page (From : Page_Record) return File_And_Page is
   begin
      return From.Header.Location;
   end Get_File_And_Page;

   --------------------
   -- Get_File_Index --
   --------------------

   function Get_File_Index (From : Page) return File_Index is
   begin
      return Get_File_Index (From.all);
   end Get_File_Index;

   --------------------
   -- Get_File_Index --
   --------------------

   function Get_File_Index (From : Page_Record) return File_Index is
   begin
      return Get_File (From.Header.Location);
   end Get_File_Index;

   --------------------
   -- Get_Page_Index --
   --------------------

   function Get_Page_Index (From : Page) return Page_Index is
   begin
      return Get_Page_Index (From.all);
   end Get_Page_Index;

   --------------------
   -- Get_Page_Index --
   --------------------

   function Get_Page_Index (From : Page_Record) return Page_Index is
   begin
      return Get_Page (From.Header.Location);
   end Get_Page_Index;

   -------------------
   -- Get_Page_Type --
   -------------------

   function Get_Page_Type (From : Page)
                          return Marlowe.Page_Types.Page_Type
   is
   begin
      return Page_Types.Page_Type'Val (From.Header.Identity);
   end Get_Page_Type;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item     : in Page;
                         Identity : in Marlowe.Page_Types.Page_Type;
                         Location : in File_And_Page)
   is
   begin
      Item.Header   := (Header_Magic, 0,
                        Marlowe.Page_Types.Page_Type'Pos (Identity),
                        Location);
      Item.Tail     := (Unused => 0, Magic => Tail_Magic);
      Item.Contents := (others => 0);
   end Initialise;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out Page) is
      procedure Free is new Ada.Unchecked_Deallocation (Page_Record, Page);
   begin
      if Item /= null then
         if Marlowe.Allocation.Debug_Allocation then
            Marlowe.Allocation.Deallocate (Item.all'Size, "page");
         end if;
         Free (Item);
      end if;
   end Release;

   --------------
   -- Validate --
   --------------

   function Validate (Item : Page_Record) return Boolean is
   begin
      return Item.Header.Magic = Header_Magic and
        Item.Tail.Magic = Tail_Magic;
   end Validate;

end Marlowe.Pages;
