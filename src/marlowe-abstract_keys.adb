------------------------------------------------------------------------------
--                                                                          --
--                        MARLOWE DATABASE LIBRARY                          --
--                                                                          --
--                         M A R L O W E . K E Y S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

with Ada.Sequential_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Table;

with Marlowe.Exceptions;
with Marlowe.Configuration;
with Marlowe.Trace;

package body Marlowe.Abstract_Keys is

   Tracing : Boolean := False;

   type Key_Entry is
      record
         Element        : Database_Index;
         Key            : Key_Type;
      end record;

   type Find_Metric is (Exact, At_Least, At_Most);

   package Key_Table is
      new GNAT.Table (Key_Entry, Natural, 1, 256, 100);

   package Key_Entry_IO is
      new Ada.Sequential_IO (Key_Entry);

   function Find (Key    : Key_Type;
                  Metric : Find_Metric := Exact)
                 return Natural;

   function Find (Key     : Key_Type;
                  Element : Database_Index)
                 return Natural;

   -------------------
   -- Empty_Context --
   -------------------

   function Empty_Context return Key_Context is
      Result : Key_Context;
   begin
      Result.Element := 0;
      Result.Scratch := 0;
      Result.Uses_Key := False;
      return Result;
   end Empty_Context;

   ------------
   -- Insert --
   ------------

   procedure Insert (Key     : in Key_Type;
                     Element : in Database_Index)
   is
      Position : Natural;
      New_Key  : constant Key_Entry := (Element => Element,
                                        Key     => Key);
   begin

      if Unique and then Find (Key) /= 0 then
         raise Marlowe.Exceptions.Duplicate_Key;
      end if;

      Key_Table.Increment_Last;
      if Key_Table.Last = 1 then
         Position := 1;
      else
         Position := Key_Table.Last;
         while Position > 1 and then
           Key < Key_Table.Table (Position - 1).Key
         loop
            Key_Table.Table (Position) := Key_Table.Table (Position - 1);
            Position := Position - 1;
         end loop;

      end if;

      Key_Table.Table (Position) := New_Key;

   end Insert;

   ----------
   -- Find --
   ----------

   function Find (Key    : Key_Type;
                  Metric : Find_Metric := Exact)
                 return Natural
   is
      Low, Middle, High : Natural;
      Test              : Key_Type;
   begin

      if Key_Table.Last = 0 then
         return 0;
      end if;

      Low := 1;
      High := Key_Table.Last;
      while Low <= High loop
         Middle := (Low + High) / 2;
         Test := Key_Table.Table (Middle).Key;
         if Key = Test then
            exit;
         elsif Key < Test then
            High := Middle - 1;
         else
            Low := Middle + 1;
         end if;
      end loop;

      if Tracing then
         Marlowe.Trace.Trace ("Stopped at position" &
                              Natural'Image (Low) &
                              Natural'Image (Middle) &
                              Natural'Image (High));
      end if;

      if Low <= High then
         if not Unique then
            case Metric is
               when Exact | At_Least =>
                  while Middle > 1 and then
                    Key = Key_Table.Table (Middle - 1).Key
                  loop
                     Middle := Middle - 1;
                  end loop;
               when At_Most =>
                  while Middle < Key_Table.Last and then
                    Key = Key_Table.Table (Middle + 1).Key
                  loop
                     Middle := Middle + 1;
                  end loop;
            end case;
         end if;

         return Middle;

      else

         case Metric is
            when Exact =>
               return 0;
            when At_Least =>
               if Low >= Key_Table.Last then
                  return 0;
               elsif not Unique then
                  while Low > 1 and then
                    Key_Table.Table (Low - 1) = Key_Table.Table (Low)
                  loop
                     Low := Low - 1;
                  end loop;
               end if;
               return Low;

            when At_Most =>
               if High = 0 then
                  return 0;
               elsif not Unique then
                  while High < Key_Table.Last and then
                    Key_Table.Table (High + 1) = Key_Table.Table (High)
                  loop
                     High := High + 1;
                  end loop;
               end if;
               return High;
         end case;
      end if;

   end Find;

   ----------
   -- Find --
   ----------

   function Find (Key     : Key_Type;
                  Element : Database_Index)
                 return Natural
   is
      Location : Natural := Find (Key);
   begin

      if Location = 0 then
         return 0;
      end if;

      while Location <= Key_Table.Last and then
        (Key = Key_Table.Table (Location).Key and then
         Element /= Key_Table.Table (Location).Element)
      loop
         Location := Location + 1;
      end loop;

      if Location <= Key_Table.Last and then
        Key = Key_Table.Table (Location).Key
      then
         return Location;
      else
         return 0;
      end if;
   end Find;

   ------------------------
   -- First_With_Element --
   ------------------------

   function First_With_Element (Key      : Key_Type;
                                Element  : Database_Index;
                                Use_Key  : Boolean)
                               return Key_Context
   is
      Location : Natural := Find (Key);
      Result   : Key_Context;
   begin

      if Location = 0 then
         if Tracing then
            Marlowe.Trace.Trace ("First_With_Element: key not found");
         end if;
         Result.Element := 0;
         Result.Scratch := 0;
         return Result;
      end if;

      while Location <= Key_Table.Last and then
        (Key_Table.Table (Location).Key = Key and then
         Key_Table.Table (Location).Element /= Element)
      loop
         Location := Location + 1;
      end loop;

      if Location <= Key_Table.Last and then
        Key_Table.Table (Location).Element = Element
      then
         Result.Element  := Element;
         Result.Scratch  := Location;
         Result.Key      := Key_Table.Table (Location).Key;
         Result.Uses_Key := Use_Key;
         return Result;
      else
         Result.Element := 0;
         return Result;
      end if;
   end First_With_Element;

   ----------
   -- Open --
   ----------
   procedure Open is
      use Key_Entry_IO;
      File : File_Type;
   begin
      Key_Table.Init;
      Open (File, In_File,
            Marlowe.Configuration.Working_Directory & Key_Name & ".key");
      while not End_Of_File (File) loop
         declare
            Ent : Key_Entry;
         begin
            Read (File, Ent);
            Key_Table.Append (Ent);
         end;
      end loop;
      Close (File);
   end Open;

   ------------
   -- Create --
   ------------
   procedure Create is
      use Key_Entry_IO;
      File : File_Type;
   begin
      Key_Table.Init;
      Create (File, Out_File,
              Marlowe.Configuration.Working_Directory & Key_Name & ".key");
      Close (File);
   end Create;

   -----------
   -- Close --
   -----------
   procedure Close is
      use Key_Entry_IO;
      File : File_Type;
   begin
      Create (File, Out_File,
              Marlowe.Configuration.Working_Directory & Key_Name & ".key");
      for I in 1 .. Key_Table.Last loop
         Write (File, Key_Table.Table (I));
      end loop;
      Close (File);
      Key_Table.Init;
   end Close;

   -----------
   -- Valid --
   -----------

   function Valid (Context : Key_Context) return Boolean is
   begin
      return Context.Element /= 0;
   end Valid;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (Context : Key_Context) return Database_Index is
   begin
      return Context.Element;
   end Get_Element;


   ------------
   -- Change --
   ------------
   procedure Change (Old_Key : in Key_Type;
                     New_Key : in Key_Type;
                     Element : in Database_Index)
   is
      Index : constant Natural := Find (Old_Key, Element);
   begin

      if Index /= 0 then
         for I in Index .. Key_Table.Last - 1 loop
            Key_Table.Table (I) := Key_Table.Table (I + 1);
         end loop;
         Key_Table.Decrement_Last;
         Insert (New_Key, Element);
      else
         Marlowe.Trace.Trace ("Change didn't find old key, dumping table ...");
         for I in 1 .. Key_Table.Last loop
            Marlowe.Trace.Trace ("   " & I'Img &
                                 Key_Table.Table (I).Element'Img);
         end loop;
         declare
            Element_Key       : Key_Type;
            Element_Key_Index : Natural := 0;
            Got_Element_Key : Boolean := False;
         begin
            for I in 1 .. Key_Table.Last loop
               if Key_Table.Table (I).Element = Element then
                  Element_Key := Key_Table.Table (I).Key;
                  Element_Key_Index := I;
                  Got_Element_Key := True;
                  exit;
               end if;
            end loop;

            raise Marlowe.Exceptions.Key_Error;
         end;
      end if;

   end Change;

   -----------
   -- First --
   -----------

   function First return Key_Context is
   begin
      if Key_Table.Last > 0 then
         return (Element  => Key_Table.Table (1).Element,
                 Scratch  => 1,
                 Uses_Key => False,
                 Key      => Key_Table.Table (1).Key);
      end if;

      declare
         Result : Key_Context;
      begin
         Result.Element := 0;
         return Result;
      end;

   end First;

   -----------
   -- First --
   -----------
   function First (Key : Key_Type) return Key_Context is
      Result   : Key_Context;
      Location : Natural;
   begin
      Location := Find (Key);
      if Location /= 0 and then
        Key_Table.Table (Location).Key = Key
      then
         Result := (Element  => Key_Table.Table (Location).Element,
                    Scratch  => Location,
                    Uses_Key => True,
                    Key      => Key_Table.Table (Location).Key);
      else
         Result.Element := 0;
      end if;
      return Result;
   end First;

   -------------------
   -- First_Greater --
   -------------------
   function First_Greater (Key : Key_Type) return Key_Context is
      Result   : Key_Context;
      Location : Natural;
   begin
      Location := Find (Key, At_Least);

      if Location = 0 then
         Result.Element   := 0;
         Result.Scratch   := 0;
         Result.Uses_Key  := False;
      else
         Result.Element   := Key_Table.Table (Location).Element;
         Result.Scratch   := Location;
         Result.Uses_Key  := False;
      end if;

      return Result;

   end First_Greater;

   ----------
   -- Last --
   ----------
   function Last return Key_Context is
      Result   : Key_Context;
      Location : constant Natural      := Key_Table.Last;
   begin

      if Key_Table.Last = 0 then
         return Result;
      end if;

      Result.Element  := Key_Table.Table (Key_Table.Last).Element;
      Result.Scratch  := Key_Table.Last;
      Result.Uses_Key := False;

      return Result;
   end Last;

   ----------
   -- Last --
   ----------
   function Last (Key : Key_Type) return Key_Context is
      Result : Key_Context;
   begin
      --  FIXME: implement this
      Result.Element := 0;
      return Result;
   end Last;

   ---------------
   -- Last_Less --
   ---------------
   function Last_Less (Key : Key_Type) return Key_Context is
      Result : Key_Context;
      Location : Natural;
   begin

      Location := Find (Key, At_Most);

      if Location = 0 then
         Result.Element   := 0;
         Result.Scratch   := 0;
         Result.Uses_Key  := False;
      else
         Result.Element   := Key_Table.Table (Location).Element;
         Result.Scratch   := Location;
         Result.Uses_Key  := False;
      end if;

      return Result;

   end Last_Less;

   ----------
   -- Next --
   ----------
   function Next (Current : Key_Context) return Key_Context is
      Result : Key_Context := Current;
   begin
      if Key_Table.Last = 0 then
         Result.Element := 0;
         return Result;
      end if;

      Result.Scratch := Result.Scratch + 1;

      if Result.Scratch > Key_Table.Last then
         Result.Element := 0;
         return Result;
      end if;

      if Result.Uses_Key and then
         Result.Key /= Key_Table.Table (Result.Scratch).Key
      then
         Result.Element := 0;
         return Result;
      end if;

      Result.Element := Key_Table.Table (Result.Scratch).Element;
      return Result;

   end Next;

   --------------
   -- Previous --
   --------------
   function Previous (Current : Key_Context) return Key_Context is
      Result : Key_Context := Current;
   begin

      if Key_Table.Last = 0 or Current.Scratch = 1 then
         Result.Element := 0;
         return Result;
      end if;

      Result.Scratch := Result.Scratch - 1;

      if Result.Uses_Key and then
         Result.Key /= Key_Table.Table (Result.Scratch).Key
      then
         Result.Element := 0;
         return Result;
      end if;

      Result.Element := Key_Table.Table (Result.Scratch).Element;
      return Result;

   end Previous;

   ------------
   -- Delete --
   ------------

   procedure Delete (Key : in out Key_Context) is
   begin
      if Key.Scratch = 0 then
         Marlowe.Trace.Trace ("Scratch = 0, dumping table ...");
         for I in 1 .. Key_Table.Last loop
            Marlowe.Trace.Trace ("   " & I'Img &
                                 Key_Table.Table (I).Element'Img);
         end loop;
      end if;

      for I in Key.Scratch .. Key_Table.Last - 1 loop
         Key_Table.Table (I) := Key_Table.Table (I + 1);
      end loop;
      Key_Table.Decrement_Last;
   end Delete;

   -----------
   -- Trace --
   -----------

   procedure Trace (On : Boolean) is
   begin
      Tracing := On;
   end Trace;

end Marlowe.Abstract_Keys;

