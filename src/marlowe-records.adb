------------------------------------------------------------------------------
--                                                                          --
--                        MARLOWE DATABASE LIBRARY                          --
--                                                                          --
--                      M A R L O W E . R E C O R D S                       --
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

with Ada.Direct_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

with WL.Trace;

with Marlowe.Exceptions;
with Marlowe.Configuration;

package body Marlowe.Records is

   type Element_Header is
      record
         Deleted : Boolean;
      end record;

   type Full_Element_Type is
      record
         Header    : Element_Header;
         Contents  : Element_Type;
      end record;

   package Element_IO is
      new Ada.Direct_IO (Full_Element_Type);

   File : Element_IO.File_Type;
   Database_Name : Ada.Strings.Unbounded.Unbounded_String;

   procedure Log (Message : String);

   ----------
   -- Open --
   ----------
   procedure Open is
      use Element_IO;
   begin
      Open (File, Inout_File,
            Marlowe.Configuration.Working_Directory & Name & ".dat");
      Database_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Open;

   ------------
   -- Create --
   ------------
   procedure Create is
      use Element_IO;
   begin
      Create (File, Inout_File,
              Marlowe.Configuration.Working_Directory & Name & ".dat");
   end Create;

   -----------
   -- Close --
   -----------
   procedure Close is
   begin
      Element_IO.Close (File);
   end Close;

   ----------
   -- Read --
   ----------
   procedure Read (From_Index : in  Database_Index;
                   Element    : out Element_Type)
   is
      Full_Element : Full_Element_Type;
   begin
      if From_Index = 0 then
         Ada.Exceptions.Raise_Exception
           (Marlowe.Exceptions.Index_Error'Identity,
            "attempted to read from index 0");
      end if;

      Element_IO.Read (File, Full_Element,
                       Element_IO.Positive_Count (From_Index));
      Element := Full_Element.Contents;
   end Read;

   -----------
   -- Write --
   -----------
   procedure Write (To_Index  : in  Database_Index;
                    Element   : in  Element_Type)
   is
      Full_Element : Full_Element_Type :=
        (Header     => (Deleted => False),
         Contents   => Element);
   begin
      if To_Index = 0 then
         Ada.Exceptions.Raise_Exception
           (Marlowe.Exceptions.Index_Error'Identity,
            "attempted to write to index 0");
      end if;

      Element_IO.Write (File, Full_Element,
                        Element_IO.Positive_Count (To_Index));
   end Write;

   ------------
   -- Delete --
   ------------
   procedure Delete (Index : Database_Index) is
      Full_Element : Full_Element_Type;
   begin
      Element_IO.Read (File, Full_Element,
                       Element_IO.Positive_Count (Index));
      if Full_Element.Header.Deleted then
         raise Marlowe.Exceptions.Double_Delete;
      end if;

      Full_Element.Header.Deleted := True;
      Element_IO.Write (File, Full_Element,
                        Element_IO.Positive_Count (Index));
   end Delete;

   -------------
   -- Deleted --
   -------------
   function Deleted (Index : in Database_Index) return Boolean is
      Full_Element : Full_Element_Type;
   begin
      Element_IO.Read (File, Full_Element,
                       Element_IO.Positive_Count (Index));
      return Full_Element.Header.Deleted;
   end Deleted;

   -----------
   -- Valid --
   -----------
   function Valid   (Index : in Database_Index) return Boolean is
      use type Element_IO.Positive_Count;
   begin
      Log ("valid: index =" & Index'Img);
      Log ("valid: size  =" &
           Element_IO.Positive_Count'Image (Element_IO.Size (File)));

      return Index > 0 and then
        Element_IO.Positive_Count (Index) <=
        Element_IO.Size (File);
   end Valid;


   ---------------
   -- New_Index --
   ---------------
   function New_Index return Database_Index is
      use type Element_IO.Positive_Count;
      Location : Element_IO.Positive_Count;
      Scratch  : Full_Element_Type;
   begin
      Location := Element_IO.Size (File) + 1;
      Scratch.Header := (Deleted   => False);
      Element_IO.Write (File, Scratch, Location);
      return Database_Index (Location);
   end New_Index;

   ---------------
   -- New_Block --
   ---------------
   function New_Block (Count : Positive) return Database_Index is
      use type Element_IO.Positive_Count;
      Scratch  : Full_Element_Type;
      Location : Element_IO.Positive_Count;
   begin
      Scratch.Header := (Deleted => False);
      Location := Element_IO.Size (File) + 1;
      for I in 1 .. Count loop
         Element_IO.Write (File, Scratch,
                           Location + Element_IO.Positive_Count (I) - 1);
      end loop;
      return Database_Index (Location);
   end New_Block;

   ----------------
   -- Read_Block --
   ----------------
   function Read_Block (Start_Index   : Database_Index;
                        Size          : Positive)
     return Array_Of_Elements
   is
      Result : Array_Of_Elements (1 .. Size);
   begin
      for I in Result'Range loop
         Read (Start_Index + Database_Index (I) - 1,
               Result (I));
      end loop;
      return Result;
   end Read_Block;

   -----------------
   -- Write_Block --
   -----------------
   procedure Write_Block (Start_Index : Database_Index;
                          Block       : Array_Of_Elements)
   is
   begin
      for I in Block'Range loop
         Write (Start_Index + Database_Index (I) -
                Database_Index (Block'First),
                Block (I));
      end loop;
   end Write_Block;

   ---------
   -- Log --
   ---------
   procedure Log (Message : String) is
   begin
      WL.Trace.Put_Line (4, "Marlowe: " &
                         Ada.Strings.Unbounded.To_String (Database_Name) &
                         ": " & Message);
   end Log;

end Marlowe.Records;

