private with Ada.Containers.Vectors;

package Semantic_Versioning.Basic with Preelaborate is

   --  Collections of versions (usually a compatible subset). These basic sets
   --  only allow "and" conditions.

   type Version_Set is tagged private;

   type Result (Valid  : Boolean;
                Length : Natural) is
      record
         case Valid is
            when True  => Set   : Version_Set;
            when False => Error : String (1 .. Length);
         end case;
      end record;

   Any : constant Version_Set;

   function Image_Ada (VS : Version_Set) return String;
   --  Ada-like textual representation.
   --  E.g., "Within_Major ("1.0.0") and Except ("1.0.5")"

   function Image_Abbreviated (VS             : Version_Set;
                               Unicode        : Boolean := False;
                               Implicit_Equal : Boolean := False) return String;
   --  '&' separated; e.g. "^1.0.0 & ≠1.0.5"
   --  If Unicode, the operator can be ≠, etc
   --  If implicit equal, "=" will be omitted

   function Image (VS             : Version_Set;
                   Unicode        : Boolean := False;
                   Implicit_Equal : Boolean := False) return String
                   renames Image_Abbreviated;

   function To_Set (S       : Version_String;
                    Relaxed : Boolean := False;
                    Unicode : Boolean := True) return Version_Set;
   -- Parses a single version set from a single restriction representation:
   -- The following operators are recognized:
   --   = /= ≠ > >= ≥ < ≤ <= ~ ^, with the meanings given in the following functions.
   -- In addition, a plain version is equivalent to =, and "any", "*" is any version.

   function Parse (S       : String;
                   Relaxed : Boolean := False;
                   Unicode : Boolean := True) return Result;
   --  Parse an expression possibly containing several sets, "&"-separated.

   function Value (S       : String;
                   Relaxed : Boolean := False;
                   Unicode : Boolean := True) return Version_Set;
   --  As Parse, but raises Malformed_Error with Error as message instead of
   --  returning a Result.

   function At_Least  (V : Version) return Version_Set; -- >= ≥
   function At_Most   (V : Version) return Version_Set; -- <= ≤
   function Less_Than (V : Version) return Version_Set; -- <
   function More_Than (V : Version) return Version_Set; -- >
   function Exactly   (V : Version) return Version_Set; -- =
   function Except    (V : Version) return Version_Set; -- /= ≠

   function Within_Major (V : Version) return Version_Set;
   -- The "^" caret operator, any version from V up to Next_Major (V)

   function Within_Minor (V : Version) return Version_Set;
   -- Similar to "~" tilde operator, any version from V up to Next_Minor (V)
   -- BUT note that it is always up to minor (unlike usual ~ implementations)

   function "and" (VS1, VS2 : Version_Set) return Version_Set;

   function Contains  (VS : Version_Set; V : Version) return Boolean;
   function Is_In     (V : Version; VS : Version_Set) return Boolean;
   function Satisfies (V : Version; VS : Version_Set) return Boolean renames Is_In;

   function Is_Single_Version (VS : Version_Set) return Boolean;
   --  True when VS contains a single Exactly restriction

   --  Iteration over version sets contents

   type Conditions is
     (At_Least, At_Most, Exactly, Except, Within_Major, Within_Minor);

   function Operator (Condition      : Conditions;
                      Unicode        : Boolean := False;
                      Implicit_Equal : Boolean := False) return String;
   --  Returns a short string with the visible operator: =, /=, ~, ...
   --  If Unicode, the operator can be ≠, etc
   --  If implicit equal, "=" will be omitted

   type Restriction is private;

   function Operator_Image (R              : Restriction;
                            Unicode        : Boolean := False;
                            Implicit_Equal : Boolean := False) return String;
   --  Image using operator (e.g., <=1.0.1, ~2.0.0, /=0.1.2)
   --  See Image (Set) for Unicode, Implicit_Equal meaning

   function Condition  (R : Restriction) return Conditions;
   function On_Version (R : Restriction) return Version;

   function Length  (VS : Version_Set) return Natural; -- 0 is Any!
   function Element (VS : Version_Set; I : Positive) return Restriction;

private

   type Restriction is record
      Condition  : Conditions;
      On_Version : Version;
   end record;

   function Condition  (R : Restriction) return Conditions is (R.Condition);
   function On_Version (R : Restriction) return Version is (R.On_Version);

   function Satisfies (V : Version; R : Restriction) return Boolean;

   package Restrictions is new Ada.Containers.Vectors (Positive, Restriction);

   type Version_Set is new Restrictions.Vector with null record;

   --  Generator functions

   function At_Least  (V : Version) return Version_Set is (To_Vector ((At_Least, V), 1));
   function At_Most   (V : Version) return Version_Set is (To_Vector ((At_Most, V), 1));
   function Exactly   (V : Version) return Version_Set is (To_Vector ((Exactly, V), 1));
   function Except    (V : Version) return Version_Set is (To_Vector ((Except, V), 1));

   function Within_Major (V : Version) return Version_Set is (To_Vector ((Within_Major, V), 1));
   function Within_Minor (V : Version) return Version_Set is (To_Vector ((Within_Minor, V), 1));

   --  Secondary functions

   function Less_Than (V : Version) return Version_Set is (At_Most (V) and Except (V));
   function More_Than (V : Version) return Version_Set is (At_Least (V) and Except (V));

   Any : constant Version_Set := (Restrictions.Empty_Vector with null record);

   function "and" (VS1, VS2 : Version_Set) return Version_Set is (VS1 & VS2);

   function Length  (VS : Version_Set) return Natural is
     (Natural (Restrictions.Vector (VS).Length));

   function Element (VS : Version_Set; I : Positive) return Restriction is
     (VS (I));

   function Operator (Condition      : Conditions;
                      Unicode        : Boolean := False;
                      Implicit_Equal : Boolean := False) return String is
     (case Condition is
         when At_Least     => (if Unicode then "≥" else ">="),
         when At_Most      => (if Unicode then "≤" else "<="),
         when Exactly      => (if Implicit_Equal then "" else "="),
         when Except       => (if Unicode then "≠" else "/="),
         when Within_Major => "^",
         when Within_Minor => "~");

   function Operator_Image (R              : Restriction;
                            Unicode        : Boolean := False;
                            Implicit_Equal : Boolean := False) return String is
     (Operator (R.Condition,
                Unicode,
                Implicit_Equal) &
        Image (R.On_Version));

   function Contains  (VS : Version_Set; V : Version) return Boolean is
     (Is_In (V, VS));

   function Is_Single_Version (VS : Version_Set) return Boolean is
     (VS.Length = 1
      and then VS.First_Element.Condition = Exactly);

end Semantic_Versioning.Basic;
