------------------------------------------------------------------------------
--                                                                          --
--                        MARLOWE DATABASE LIBRARY                          --
--                                                                          --
--                      M A R L O W E . R E C O R D S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.1.1 $                              --
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

generic
   Name : String;
   type Element_Type is private;
package Marlowe.Records is

   type Array_Of_Elements is array (Positive range <>) of Element_Type;

   procedure Open;
   procedure Create;
   procedure Close;

   procedure Read (From_Index : in  Database_Index;
                   Element    : out Element_Type);

   procedure Write (To_Index  : in  Database_Index;
                    Element   : in  Element_Type);

   procedure Delete (Index : in Database_Index);

   function Deleted (Index : in Database_Index) return Boolean;
   function Valid   (Index : in Database_Index) return Boolean;

   function New_Index return Database_Index;
   function New_Block (Count : Positive) return Database_Index;


   function Read_Block (Start_Index   : Database_Index;
                        Size          : Positive)
     return Array_Of_Elements;

   procedure Write_Block (Start_Index : Database_Index;
                          Block       : Array_Of_Elements);

end Marlowe.Records;

