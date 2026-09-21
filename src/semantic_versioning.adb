with Ada.Strings.Maps;

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

package body Semantic_Versioning is

   -----------------
   -- Begins_With --
   -----------------
   --  See if a substring is at the beginning of another, subrange-safe
   function Begins_With (S : String; Pattern : String) return Boolean is
        (if Pattern'Length >= S'Length then False -- We need at least one extra character for the actual version
         else S (S'First .. S'First + Pattern'Length - 1) = Pattern);

   ----------------------------
   -- Begins_With_Relational --
   ----------------------------

   function Begins_With_Relational (S       : String;
                                    Unicode : Boolean := False) return Boolean is
     ((S'Length >= 1 and then S (S'First) in '<' | '>' | '=' | '/' | '~' | '^')
       or else
         (Unicode and then
            (Begins_With (S, U ("≠")) or else
             Begins_With (S, U ("≥")) or else
             Begins_With (S, U ("≤")))));

   -----------
   -- Parse --
   -----------

   function Parse (Description : Version_String;
                   Relaxed     : Boolean := False) return Version
   is
      Next : Positive := Description'First;
      V    : Version;

      type Tokens is      (Number, Dot, Minus, Plus, Other, Done);
      type Foreseeable is (Major, Minor, Patch, Nothing);

      To_See : Foreseeable := Major;

      ---------------
      -- Next_Char --
      ---------------

      function Next_Char return Character is
         (Description (Next));

      ----------------
      -- Next_Token --
      ----------------

      function Next_Token (At_Position : Natural := Next) return Tokens is
        (if At_Position > Description'Last
         then Done
         else
           (case Description (At_Position) is
               when '.' => Dot,
               when '+' => Plus,
               when '-' => Minus,
               when '0' .. '9' => Number,
               when others     => Other));

      ----------------
      -- Eat_Number --
      ----------------

      function Eat_Number return Point is
         Last : Natural := Next + 1;
      begin
         if Next <= Description'Last and then Next_Token (Next) /= Number then
            raise Malformed_Input with "Empty point number: " & Description (Next);
         elsif Next > Description'Last then
            raise Malformed_Input with "Empty point number (EOF)";
         end if;

         while Last <= Description'Last and then Next_Token (Last) = Number loop
            Last := Last + 1;
         end loop;

         return Number : constant Point := Point'Value (Description (Next .. Last - 1)) do
            Next := Last;
         end return;
      end Eat_Number;

      --------------
      -- Eat_Char --
      --------------

      procedure Eat_Char is
      begin
         Next := Next + 1;
      end Eat_Char;

      ------------------
      -- Accept_Build --
      ------------------

      procedure Accept_Build is
      begin
         V.Build := Ustrings.To_Unbounded_String (Description (Next .. Description'Last));
         Next    := Description'Last + 1;
      end Accept_Build;

      ----------------
      -- Accept_Pre --
      ----------------

      procedure Accept_Pre is
         Last : Natural := Next + 1;
      begin
         if Next > Description'Last then
            raise Malformed_Input with "Empty pre-release part: " & Description;
         end if;

         while Last <= Description'Last and then Description (Last) /= '+' loop
            Last := Last + 1;
         end loop;

         V.Pre_Release := UStrings.To_Unbounded_String (Description (Next .. Last - 1));
         Next := Last;

         case Next_Token is
            when Done => null;
            when Plus =>
               Eat_Char;
               Accept_Build;
            when others =>
               raise Program_Error with "Unexpected token after pre-release: " & Description;
         end case;
      end Accept_Pre;

      -------------------
      -- Accept_Number --
      -------------------

      procedure Accept_Number is
      begin
         case To_See is
            when Major => V.Major := Eat_Number;
            when Minor =>
               begin
                  V.Minor := Eat_Number;
               exception
                  when Malformed_Input =>
                     if Relaxed then
                        Next := Next - 1; -- We need to re-eat the '.'
                        Accept_Build;
                     else
                        raise;
                     end if;
               end;
            when Patch =>
               begin
                  V.Patch := Eat_Number;
               exception
                  when Malformed_Input =>
                     if Relaxed then
                        Next := Next - 1; -- We need to re-eat the '.'
                        Accept_Build;
                     else
                        raise;
                     end if;
               end;
            when others => raise Malformed_Input with "All foreseeable points already seen";
         end case;
         To_See := Foreseeable'Succ (To_See);

         case Next_Token is
            when Number => raise Program_Error with "Number found after eating number";
            when Dot    =>
               if To_See = Nothing then
                  if Relaxed then
                     Eat_Char;
                     Accept_Build;
                  else
                     raise Malformed_Input with "Too many points in version: " & Description;
                  end if;
               else
                  Eat_Char;
                  Accept_Number;
               end if;
            when Minus  =>
               Eat_Char;
               Accept_Pre;
            when Plus   =>
               Eat_Char;
               Accept_Build;
            when Other  =>
               if Relaxed Then
                  Accept_Build;
               else
                  raise Malformed_Input with "Invalid separator after major number: " & Next_Char;
               end if;
            when Done   => null;
         end case;
      end Accept_Number;

   begin
      case Next_Token is
         when Number => Accept_Number;
         when others => raise Malformed_Input with "Major number expected";
      end case;

      return V;
   end Parse;

   ----------------
   -- Is_Numeric --
   ----------------
   --  True if S is a non-empty string of ASCII digits, per Semver 2.0.0 §11.4
   function Is_Numeric (S : String) return Boolean is
     (S /= "" and then (for all C of S => C in '0' .. '9'));

   type Field_Order is (Lower, Equal, Higher);

   --------------------
   -- Compare_Fields --
   --------------------
   --  Compares two dot-separated pre-release identifiers per Semver 2.0.0 §11.4:
   --  numeric identifiers compare numerically (arbitrary precision, no overflow),
   --  alphanumeric identifiers compare lexicographically (ASCII order), and a
   --  numeric identifier always has lower precedence than an alphanumeric one.
   function Compare_Fields (L, R : String) return Field_Order is
   begin
      if Is_Numeric (L) and then Is_Numeric (R) then
         declare
            --  Trim leading zeros (keeping at least one digit) so that
            --  identifiers of arbitrary size compare correctly without
            --  going through a bounded integer type.
            function Trim_Zeros (S : String) return String is
              (if S'Length > 1 and then S (S'First) = '0'
               then Trim_Zeros (S (S'First + 1 .. S'Last))
               else S);
            L_Trim : constant String := Trim_Zeros (L);
            R_Trim : constant String := Trim_Zeros (R);
         begin
            if L_Trim'Length /= R_Trim'Length then
               return (if L_Trim'Length < R_Trim'Length then Lower else Higher);
            elsif L_Trim = R_Trim then
               return Equal;
            else
               return (if L_Trim < R_Trim then Lower else Higher);
            end if;
         end;
      elsif Is_Numeric (L) then
         return Lower;  -- Numeric identifiers are always lower than alphanumeric ones
      elsif Is_Numeric (R) then
         return Higher;
      elsif L = R then
         return Equal;
      else
         return (if L < R then Lower else Higher);
      end if;
   end Compare_Fields;

   ---------------------------
   -- Less_Than_Pre_Release --
   ---------------------------

   function Less_Than_Pre_Release (L, R : String) return Boolean is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Dot : constant Character_Set := To_Set (".");
      L_First, L_Last : Natural := L'First - 1;
      R_First, R_Last : Natural := R'First - 1;
   begin
      --  Special case if one of them is not really a pre-release:
      if L /= "" and then R = "" then
         return True;
      elsif L = "" and then R /= "" then
         return False;
      end if;

      loop
         if R_Last = R'Last then -- R depleted, at most L is depleted too
            return False;
         elsif L_Last = L'Last then -- L depleted, hence is <
            return True;
         else
            null; -- There are more tokens to compare
         end if;

         Find_Token (L, Dot, L_Last + 1, Outside, L_First, L_Last);
         Find_Token (R, Dot, R_Last + 1, Outside, R_First, R_Last);

         if R_Last = 0 then
            return False; -- L can't be less; at most equal (both empty)
         elsif L_Last = 0 then
            return True;  -- Since R is not exhausted but L is.
         else -- Field against field
            case Compare_Fields (L (L_First .. L_Last), R (R_First .. R_Last)) is
               when Lower  => return True;
               when Higher => return False;
               when Equal  => null; -- Try next fields
            end case;
         end if;
      end loop;
   end Less_Than_Pre_Release;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Version) return Boolean is
      use UStrings;
   begin
      if L.Major < R.Major then
         return True;
      elsif L.Major = R.Major then
         if L.Minor < R.Minor then
            return True;
         elsif L.Minor = R.Minor then
            if L.Patch < R.Patch then
               return True;
            elsif L.Patch = R.Patch then -- Pre-release versions are earlier than regular versions
               return Less_Than_Pre_Release (To_String (L.Pre_Release), To_String (R.Pre_Release));
            end if;
         end if;
      end if;

      return False; -- In all other cases
   end "<";

   --------------
   -- To_Basic --
   --------------

   function To_Basic    (V  : Version) return Basic.Version_Set is
     (Basic.Exactly (V));

   -----------------
   -- To_Extended --
   -----------------

   function To_Extended (V  : Version) return Extended.Version_Set is
     (Extended.To_Extended (To_Basic (V)));

   -----------------
   -- To_Extended --
   -----------------

   function To_Extended (VS : Basic.Version_Set) return Extended.Version_Set is
     (Extended.To_Extended (VS));

   ---------------
   -- Updatable --
   ---------------

   function Updatable (V : Version) return Extended.Version_Set is
     (if Major (V) = 0
      then To_Extended (Basic.Within_Minor (V))
      else To_Extended (Basic.Within_Major (V)));

end Semantic_Versioning;
