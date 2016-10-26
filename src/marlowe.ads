------------------------------------------------------------------------------
--                                                                          --
--                        MARLOWE DATABASE LIBRARY                          --
--                                                                          --
--                              M A R L O W E                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.1.1 $                          --
--                                                                          --
--                    Copyright (c) 2000 Fraser Wilson                      --
--                                                                          --
-- Marlowe is free software; you can redistribute it and/or modify  it  un- --
-- der  terms  of  the  GNU General Public License as published by the Free --
-- Software Foundation; either version 2, or (at  your  option)  any  later --
-- version.  Marlowe is distributed in the hope that it will be useful, but --
-- WITHOUTANY WARRANTY; without even the implied warranty of  MERCHANTABIL- --
-- ITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details. You should have received a  copy  of  the  GNU --
-- General  Public  License  distributed with Marlowe; see file COPYING. If --
-- not, write to the Free Software Foundation,  59  Temple  Place  -  Suite --
-- 330, Boston, MA 02111-1307, USA.                                         --
--                                                                          --
------------------------------------------------------------------------------

package Marlowe is

   pragma Pure (Marlowe);

   type Database_Index is mod 2**64;
   subtype Real_Database_Index is
     Database_Index range 1 .. Database_Index'Last;

   type Scan_Type is (Forward, Backward);
   type Interval_Type is (Open, Closed, Unbounded);

   Slot_Index_Bits : constant := 16;
   type Slot_Index is mod 2**Slot_Index_Bits;

   type Page_Index is private;
   type File_Index is private;

   type File_And_Page is private;
   type File_Page_And_Slot is private;

   type Table_Index is mod 2 ** 16;
   subtype Real_Table_Index is Table_Index range 1 .. Table_Index'Last;

   function Image (Item : File_And_Page) return String;
   function Image (Item : File_Page_And_Slot) return String;

   Null_File_And_Page      : constant File_And_Page;
   Null_File_Page_And_Slot : constant File_Page_And_Slot;

   function To_File_And_Page (File : File_Index;
                              Page : Page_Index)
                              return File_And_Page;

   function To_File_Page_And_Slot (File : File_Index;
                                   Page : Page_Index;
                                   Slot : Slot_Index)
                                   return File_Page_And_Slot;

   function To_File_Page_And_Slot (Page : File_And_Page;
                                   Slot : Slot_Index)
                                   return File_Page_And_Slot;

   function Get_File (From : File_And_Page) return File_Index;
   function Get_File (From : File_Page_And_Slot) return File_Index;

   function Get_Page (From : File_And_Page) return Page_Index;
   function Get_Page (From : File_Page_And_Slot) return Page_Index;

   function Get_File_And_Page
     (From : File_Page_And_Slot)
      return File_And_Page;

   function Get_Slot (From : File_Page_And_Slot) return Slot_Index;

   type Marlowe_Magic_Number is mod 2 ** 64;

   type Page_Type is mod 2 ** 16;

   subtype Internal_Page_Type is Page_Type range 0 .. 31;
   subtype External_Page_Type is Page_Type range 32 .. Page_Type'Last;

private

   Page_Index_Bits : constant := 32;
   File_Index_Bits : constant := 16;

   type Page_Index is mod 2**Page_Index_Bits;
   type File_Index is mod 2**File_Index_Bits;

   File_And_Page_Bits      : constant := 64;
   File_Page_And_Slot_Bits : constant := 64;

   type File_And_Page is mod 2**64;
   type File_Page_And_Slot is mod 2**64;

   Null_File_Page_And_Slot : constant File_Page_And_Slot := 0;
   Null_File_And_Page      : constant File_And_Page      := 0;

end Marlowe;
