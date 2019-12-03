with Ada.Characters.Handling;

with Gnat.Case_Util;

package body Semantic_Versioning.Basic is

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
          then " & " & Inner_Image (Remain)
          else "");
      end Inner_Image;

   begin
      if VS.Is_Empty then
         return "=*";
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

   function To_Set (S       : Version_String;
                    Relaxed : Boolean := False;
                    Unicode : Boolean := True) return Version_Set is
      subtype Numbers is Character range '0' .. '9';

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
      elsif Unicode and then Begins_With (S, "≠") then
         return Except (Parse (Remainder (S, "≠"), Relaxed));
      elsif Begins_With (S, ">=") then
         return At_Least (Parse (Remainder (S, ">="), Relaxed));
      elsif Unicode and then Begins_With (S, "≥") then
         return At_Least (Parse (Remainder (S, "≥"), Relaxed));
      elsif Begins_With (S, "<=") then
         return At_most (Parse (Remainder (S, "<="), Relaxed));
      elsif Unicode and then Begins_With (S, "≤") then
         return At_Most (Parse (Remainder (S, "≤"), Relaxed));
      elsif Begins_With (S, ">") then
         return More_Than (Parse (Remainder (S, ">"), Relaxed));
      elsif Begins_With (S, "<") then
         return Less_Than (Parse (Remainder (S, "<"), Relaxed));
      end if;

      --  All others
      raise Malformed_Input with "invalid set: " & S;
   end To_Set;

end Semantic_Versioning.Basic;
