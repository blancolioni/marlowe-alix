with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Marlowe.Page_Types;

package body Marlowe.Pages.Free_Map is

   ----------
   -- Free --
   ----------

   procedure Free (In_Page : Free_Map_Page;
                   Index   : Page_Index)
   is
      Page_Offset : constant Page_Index :=
        (Index - Get_Page (In_Page.Header.Location) - 1);
      Map_Offset  : constant Page_Index :=
        Page_Offset / Free_Word'Length;
      Word_Offset : constant Page_Index :=
        Page_Offset mod Free_Word'Length;
   begin
      pragma Assert (Map_Offset in In_Page.Free_Map'Range);
      pragma Assert (not In_Page.Free_Map (Map_Offset)(Word_Offset));
      In_Page.Free_Map (Map_Offset)(Word_Offset) := True;
      In_Page.Count := In_Page.Count - 1;
   end Free;

   -------------------
   -- Get_Free_Page --
   -------------------

   function Get_Free_Page (From  : Free_Map_Page) return Page_Index is
   begin
      if From.Count < Num_Pages_In_Map then
         for I in From.Free_Map'Range loop
            if From.Free_Map (I) /= Free_Word'(others => False) then
               declare
                  Value : Free_Word renames From.Free_Map (I);
               begin
                  for J in Value'Range loop
                     if Value (J) then
                        Value (J) := False;
                        From.Count := From.Count + 1;
                        return Get_Page (From.Header.Location) + 1 +
                          I * Value'Length + J;
                     end if;
                  end loop;
                  pragma Assert (False);
               end;
            end if;
         end loop;
      end if;
      return 0;
   end Get_Free_Page;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item      : in Free_Map_Page;
                         Location  : in File_And_Page)
   is
   begin
      Initialise (To_Page (Item),
                  Page_Types.Free_Map_Page_Type,
                  Location);
      Item.Count    := 0;
      Item.Free_Map := (others => (others => True));
   end Initialise;

   --------------------
   -- Next_Free_Page --
   --------------------

   function Next_Free_Page (After : Free_Map_Page) return Page_Index is
      Result : constant Page_Index :=
        Get_Page (After.Header.Location) + Num_Pages_In_Map + 1;
   begin
      if Result + Num_Pages_In_Map < Max_Pages_In_File then
         return Result;
      else
         return 0;
      end if;
   end Next_Free_Page;

   -------------
   -- Release --
   -------------

   procedure Release (Item : in out Free_Map_Page) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Free_Map_Page_Record,
                                         Free_Map_Page);
   begin
      Free (Item);
   end Release;

   -----------------
   -- To_Free_Map --
   -----------------

   function To_Free_Map_Page (Item : Page) return Free_Map_Page is
      use Marlowe.Page_Types;
      function TFM is new Ada.Unchecked_Conversion (Page, Free_Map_Page);
   begin
      pragma Assert (Get_Page_Type (Item) = Free_Map_Page_Type);
      return TFM (Item);
   end To_Free_Map_Page;

   -------------
   -- To_Page --
   -------------

   function To_Page (Item : Free_Map_Page) return Page is
      function TP is new Ada.Unchecked_Conversion (Free_Map_Page, Page);
   begin
      return TP (Item);
   end To_Page;

end Marlowe.Pages.Free_Map;

