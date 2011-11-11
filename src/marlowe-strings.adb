package body Marlowe.Strings is

   function Is_Open_Bracket (Char : Character) return Boolean;
   function Is_Close_Bracket (Char : Character) return Boolean;
   function Get_Bracket_Match (Char : Character) return Character;

   -----------------------
   -- Get_Bracket_Match --
   -----------------------

   function Get_Bracket_Match (Char : Character) return Character is
   begin
      case Char is
         when '(' =>
            return ')';
         when ')' =>
            return '(';
         when '[' =>
            return ']';
         when ']' =>
            return '[';
         when '<' =>
            return '>';
         when '>' =>
            return '<';
         when others =>
           raise Constraint_Error;
      end case;
   end Get_Bracket_Match;

   ----------------------
   -- Is_Close_Bracket --
   ----------------------

   function Is_Close_Bracket (Char : Character) return Boolean is
   begin
      case Char is
         when ']' | ')' | '>' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Close_Bracket;

   ---------------------
   -- Is_Open_Bracket --
   ---------------------

   function Is_Open_Bracket (Char : Character) return Boolean is
   begin
      case Char is
         when '[' | '(' | '<' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Open_Bracket;

   ----------
   -- Next --
   ----------

   procedure Next (Text        : in     String;
                   First, Last : in out Natural)
   is
   begin
      if Last = 0 then
         First := Text'First;
      else
         First := Last + 1;
      end if;

      if (First <= Text'Last and Last > 0) and then
        Text (First) = '''
      then
         First := First + 1;
      end if;

      while First <= Text'Last and then Text (First) = ' ' loop
         First := First + 1;
      end loop;

      if First > Text'Last then
         Last := First;
         return;
      end if;

      if Is_Open_Bracket (Text (First)) then
         declare
            Nest  : array (Character) of Natural := (others => 0);
            Count : Natural := 1;
         begin
            Nest (Text (First)) := 1;
            Last := First + 1;
            while Count /= 0 and Last <= Text'Last loop
               if Is_Open_Bracket (Text (Last)) then
                  Nest (Text (Last)) := Nest (Text (Last)) + 1;
                  Count := Count + 1;
               elsif Is_Close_Bracket (Text (Last)) then
                  declare
                     Match : constant Character :=
                       Get_Bracket_Match (Text (Last));
                  begin
                     Nest (Match) := Nest (Match) - 1;
                  end;
                  Count := Count - 1;
               end if;
               Last := Last + 1;
               exit when Nest (Text (First)) = 0;
            end loop;
            Last := Last - 1;
         end;
      elsif Text (First) = ''' then
         First := First + 1;
         Last  := First;
         loop
            exit when Last > Text'Last;
            if Text (Last) = ''' then
               if Last = Text'Last then
                  exit;
               end if;

               if Text (Last + 1) = ''' then
                  Last := Last + 1;
               else
                  exit;
               end if;
            end if;
            Last := Last + 1;
         end loop;

         Last := Last - 1;
      else
         Last := First;
         while Last <= Text'Last and then Text (Last) /= ' ' loop
            Last := Last + 1;
         end loop;
         Last := Last - 1;
      end if;
   end Next;

end Marlowe.Strings;
