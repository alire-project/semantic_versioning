with Ada.Characters.Handling;
with Ada.Strings.Maps;

with Gnat.Case_Util;

package body Semantic_Versioning is

   -------------------
   -- To_Mixed_Case --
   -------------------

   function To_Mixed_Case (S : String) return String is
   begin
      return SMC : String := S do
         GNAT.Case_Util.To_Mixed (SMC);
      end return;
   end To_Mixed_Case;

   -----------------------
   -- Image_Abbreviated --
   -----------------------

   function Image_Abbreviated (VS             : Version_Set;
                               Unicode        : Boolean := False;
                               Implicit_Equal : Boolean := False) return String
   is

      function Inner_Image (VS : Version_Set) return String is
         Cond   : constant Restriction := VS.First_Element;
         Remain : Version_Set := VS;
      begin
         Remain.Delete_First;

         return Operator_Image (Cond, Unicode, Implicit_Equal) &
         (if VS.Length > Natural'(1)
          then "," & Inner_Image (Remain)
          else "");
      end Inner_Image;

   begin
      if VS.Is_Empty then
         return "Any";
      else
         return Inner_Image (VS);
      end if;
   end Image_Abbreviated;

   ---------------
   -- Image_Ada --
   ---------------

   function Image_Ada (VS : Version_Set) return String is

      function Inner_Image (VS : Version_Set) return String is
         Cond   : constant Restriction := VS.First_Element;
         Remain : Version_Set := VS;
      begin
         Remain.Delete_First;

         return To_Mixed_Case (Cond.Condition'Img) & " (" & Image (Cond.On_Version) & ")" &
           (if VS.Length > Natural'(1) then " and " & Inner_Image (Remain) else "");
      end Inner_Image;

   begin
      if VS.Is_Empty then
         return "Any";
      else
         return Inner_Image (VS);
      end if;
   end Image_Ada;

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

         if not Relaxed then
            for C of Description (Next .. Last - 1) loop
               if C = '-' then
                  raise Malformed_Input with "Second '-' found inside pre-release part: " & Description;
               end if;
            end loop;
         end if;

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
            when Minor => V.Minor := Eat_Number;
            when Patch => V.Patch := Eat_Number;
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
      L_Num, R_Num    : Integer;
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
              -- Compare field numerically, if possible:
            declare
               L_Str : String renames L (L_First .. L_Last);
               R_Str : String renames R (R_First .. R_Last);
            begin
               L_Num := Integer'Value (L_Str);
               R_Num := Integer'Value (R_str);

               if L_Num /= R_Num then
                  return L_Num < R_Num;
               else
                  null; -- Try next fields
               end if;
            exception
               when Constraint_Error => -- Can't convert, compare lexicographically
                  if L_Str /= R_Str then
                     return L_Str < R_Str;
                  else
                     null; -- Try next fields
                  end if;
            end;
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

   -----------
   -- Is_In --
   -----------

   function Is_In (V : Version; VS : Version_Set) return Boolean is
   begin
      for R of VS loop
         if not Satisfies (V, R) then
            return False;
         end if;
      end loop;

      return True;
   end Is_In;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (V : Version; R : Restriction) return Boolean is
   begin
      case R.Condition is
         when At_Least =>
            return V = R.On_Version or else R.On_Version < V;
         when At_Most =>
            return V < R.On_Version or else V = R.On_Version;
         when Exactly =>
            return V = R.On_Version;
         when Except =>
            return V /= R.On_Version;
         when Within_Major =>
            return (R.On_Version < V or else R.On_Version = V) and then R.On_Version.Major = V.Major;
         when Within_Minor =>
            return (R.On_Version < V or else R.On_Version = V) and Then
                    R.On_Version.Major = V.Major and then R.On_Version.Minor = V.Minor;
      end case;
   end Satisfies;

   ------------
   -- To_Set --
   ------------

   function To_Set (S : Version_String; Relaxed : Boolean := False) return Version_Set is
      subtype Numbers is Character range '0' .. '9';

      --  See if a substring is at the beginning of another, subrange-safe
      function Begins_With (S : String; Pattern : String) return Boolean is
        (if Pattern'Length >= S'Length then False -- We need at least one extra character for the actual version
         else S (S'First .. S'First + Pattern'Length - 1) = Pattern);

      --  Convenience to remove the operator, whatever its length
      function Remainder (S : String; Pattern : String) return String is
         (S (S'First + Pattern'Length .. S'Last));

      package ACH renames Ada.Characters.Handling;
   begin
      --  Special cases first
      if ACH.To_Lower (S) = "any" or else S = "*" then
         return Any;
      elsif S = "" then
         raise Malformed_Input with "empty string";
      elsif S (S'First) in Numbers then
         return Exactly (Parse (S, Relaxed));
      end if;

      --  Simple cases
      declare
         Op      : constant Character := S (S'First);
         Version : constant String    := S (S'First + 1 .. S'Last);
      begin
         case Op is
            when '=' => return Exactly (Parse (Version, Relaxed));
            when '^' => return Within_Major (Parse (Version, Relaxed));
            when '~' => return Within_Minor (Parse (Version, Relaxed));
            when others => null; -- Check next cases
         end case;
      end;

      --  Rest of cases
      if Begins_With (S, "/=") then
         return Except (Parse (Remainder (S, "/="), Relaxed));
      elsif Begins_With (S, "≠") then
         return Except (Parse (Remainder (S, "≠"), Relaxed));
      elsif Begins_With (S, ">=") then
         return At_Least (Parse (Remainder (S, ">="), Relaxed));
      elsif Begins_With (S, "≥") then
         return At_Least (Parse (Remainder (S, "≥"), Relaxed));
      elsif Begins_With (S, "<=") then
         return At_most (Parse (Remainder (S, "<="), Relaxed));
      elsif Begins_With (S, "≤") then
         return At_Most (Parse (Remainder (S, "≤"), Relaxed));
      elsif Begins_With (S, ">") then
         return More_Than (Parse (Remainder (S, ">"), Relaxed));
      elsif Begins_With (S, "<") then
         return Less_Than (Parse (Remainder (S, "<"), Relaxed));
      end if;

      --  All others
      raise Malformed_Input with "invalid set: " & S;
   end To_Set;

end Semantic_Versioning;
