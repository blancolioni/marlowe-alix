------------------------------------------------------------------------------
--                                                                          --
--                        MARLOWE DATABASE LIBRARY                          --
--                                                                          --
--                M A R L O W E . C O N F I G U R A T I O N                 --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Strings.Unbounded;

package body Marlowe.Configuration is

   Working_Dir : Ada.Strings.Unbounded.Unbounded_String;

   procedure Set_Working_Directory (Directory : String) is
   begin

      if Directory (Directory'Last) /= '/' then
         Working_Dir :=
           Ada.Strings.Unbounded.To_Unbounded_String (Directory & '/');
      else
         Working_Dir :=
           Ada.Strings.Unbounded.To_Unbounded_String (Directory);
      end if;
   end Set_Working_Directory;

   function Working_Directory return String is
   begin
      return Ada.Strings.Unbounded.To_String (Working_Dir);
   end Working_Directory;

end Marlowe.Configuration;

