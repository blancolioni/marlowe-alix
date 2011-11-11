------------------------------------------------------------------------------
--                                                                          --
--                        MARLOWE DATABASE LIBRARY                          --
--                                                                          --
--                M A R L O W E . C O N F I G U R A T I O N                 --
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

package Marlowe.Configuration is

   procedure Set_Working_Directory (Directory : String);
   function Working_Directory return String;

end Marlowe.Configuration;
