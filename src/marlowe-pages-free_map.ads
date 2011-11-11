package Marlowe.Pages.Free_Map is

   type Free_Map_Page_Record is private;

   type Free_Map_Page is access all Free_Map_Page_Record;

   function To_Free_Map_Page (Item : Page) return Free_Map_Page;
   function To_Page (Item : Free_Map_Page) return Page;

   procedure Initialise (Item      : in Free_Map_Page;
                         Location  : in File_And_Page);

   procedure Release (Item : in out Free_Map_Page);

   function Get_Free_Page (From  : Free_Map_Page) return Page_Index;

   function Next_Free_Page (After : Free_Map_Page) return Page_Index;

   procedure Free (In_Page : Free_Map_Page;
                   Index   : Page_Index);

private

   type Free_Word is array (Page_Index range 0 .. 31) of Boolean;
   pragma Pack (Free_Word);
   for Free_Word'Size use 32;

   Num_Pages_In_Map : constant := Page_Contents_Bits - 32;

   type Array_Of_Free_Words is
     array (Page_Index range 0 .. Num_Pages_In_Map / 32 - 1) of Free_Word;

   type Free_Map_Page_Record is
      record
         Header    : Page_Header;
         Count     : Word_2               := 0;
         Unused    : Word_2               := 0;
         Free_Map  : Array_Of_Free_Words;
         Tail      : Page_Tail;
      end record;

   for Free_Map_Page_Record'Size use Page_Bits;

end Marlowe.Pages.Free_Map;

