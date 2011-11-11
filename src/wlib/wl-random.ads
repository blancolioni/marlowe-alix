------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                            W L . R A N D O M                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (c) 2004 Fraser Wilson                      --
--                                                                          --
-- WLib is free software; you can redistribute it and/or  modify  it  under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation; either version 2, or (at  your  option)  any  later --
-- version.  WLib  is  distributed  in the hope that it will be useful, but --
-- WITHOUTANY WARRANTY; without even the implied warranty of  MERCHANTABIL- --
-- ITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details. You should have received a  copy  of  the  GNU --
-- General  Public License distributed with WLib; see file COPYING. If not, --
-- write to the Free Software Foundation, 59  Temple  Place  -  Suite  330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

--  A quick'n'dirty interface to Ada.Numerics.Discrete_Random,
--  so that you don't have to instantiate it yourself.


package WL.Random is
   pragma Elaborate_Body;

   function Random_Number (Max : Natural) return Natural;
   function Random_Number (Min, Max : Integer) return Integer;

   procedure Randomise;

end WL.Random;
