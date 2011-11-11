------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                            W L . R A N D O M                             --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Numerics.Discrete_Random;
with Calendar;

package body WL.Random is
     package My_Random is new Ada.Numerics.Discrete_Random (Natural);

   G : My_Random.Generator;

   function Random_Number (Max : Natural) return Natural is
   begin
      if Max = 0 then
         return 0;
      else
         return My_Random.Random (G) mod Max;
      end if;
   end Random_Number;

   function Random_Number (Min, Max : Integer) return Integer is
   begin
      if Max < Min then
         return Max;
      else
         return Min + My_Random.Random (G) mod (Max - Min + 1);
      end if;
   end Random_Number;

   procedure Randomise is
      use Calendar;
      Seed : Integer;
      Now  : Calendar.Time;
   begin
      Now := Calendar.Clock;
      Seed := Integer (Seconds (Now)) * 10;
      My_Random.Reset (G, Seed);
   end Randomise;

begin
   My_Random.Reset (G, 0);
end WL.Random;
