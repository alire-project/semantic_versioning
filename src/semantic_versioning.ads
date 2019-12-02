private with Ada.Containers.Vectors;
private with Ada.Strings;
private with Ada.Strings.Fixed;
private with Ada.Strings.Unbounded;

package Semantic_Versioning with Preelaborate is

   Malformed_Input : exception;
   --  Returned whenever bad data is received. This includes, at least:
   --  * Strings that do not follow semantic versioning, in strict parsing
   --  * Unknown operators in string restrictions

   type Point is range 0 .. 99_999_999;
   --  Enough to store a YYYYMMDD as a point

   function Image (P : Point) return String;

   subtype Version_String is String
     with Dynamic_Predicate => (for all S of Version_String => S /= ' ');

   type Version is private;
   --  A version is a major, minor and patch number
   --  Optionally it may include pre-release name and build metadata, e.g.:
   --  1.2.0-alpha+c3423fab

   type Version_Set is private;

   --  A collection of versions (usually a compatible subset)

   Any : constant Version_Set;

   function New_Version (Major : Point;
                         Minor,
                         Patch : Point := 0;
                         Pre_Release,
                         Build : String := "") return Version;
   -- Refer to http://semver.org/ for the exact meaning of each part.
   -- Only the three numbers are mandatory.

   function Parse (Description : Version_String;
                   Relaxed     : Boolean := False) return Version;
   --  See Relaxed subprogram below for the meaning of Relaxed.

   function New_Version (Description : Version_String) return Version is (Parse (Description));
   function V           (Description : Version_String) return Version renames New_Version;
   function "+"         (Description : Version_String) return Version renames New_Version;
   --  These are strict parsers that will fail on versions not respecting the semver spec

   function Relaxed (Description : Version_String) return Version is (Parse (Description, Relaxed => True));
   --  This parser will attempt to follow spec as much as possible.
   --  Anything not conforming will be shoved into the pre-release (if '-' separator) or build part (otherwise)

   function Image (V : Version) return Version_String;
   --  Back to string representation

   function Image_Ada (VS : Version_Set) return String;
   --  Ada-like textual representation.
   --  E.g., "Within_Major ("1.0.0") and Except ("1.0.5")"

   function Image_Abbreviated (VS             : Version_Set;
                               Unicode        : Boolean := False;
                               Implicit_Equal : Boolean := False) return String;
   --  Comma separated; e.g. "^1.0.0,≠1.0.5"
   --  If Unicode, the operator can be ≠, etc
   --  If implicit equal, "=" will be omitted

   function "<" (L, R : Version) return Boolean;
   -- Refer to http://semver.org/ for the exact ordering. Most notably:
   -- A version with pre-release tag is earlier than its regular version.
   -- Build info is not taken into account to determine ordering.

   function "=" (L, R : Version) return Boolean;
   -- Conforming to Semver spec, the build metadata is not included in the comparison.

   function Major (V : Version) return Point;
   function Minor (V : Version) return Point;
   function Patch (V : Version) return Point;
   function Pre_Release (V : Version) return String;
   function Build (V : Version) return String;

   function Next_Patch (V : Version;
                        Pre_Release,
                        Build : String := "") return Version;

   function Next_Minor (V : Version;
                        Pre_Release,
                        Build : String := "") return Version;

   function Next_Major (V : Version;
                        Pre_Release,
                        Build : String := "") return Version;

   function To_Set (S       : Version_String;
                    Relaxed : Boolean := False;
                    Unicode : Boolean := True) return Version_Set;
   -- Parses a version set from a single restriction representation:
   -- The following operators are recognized:
   --   = /= ≠ > >= ≥ < ≤ <= ~ ^, with the meanings given in the following functions.
   -- In addition, a plain version is equivalent to =, and "any", "*" is any version.

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

   function Is_In     (V : Version; VS : Version_Set) return Boolean;
   function Satisfies (V : Version; VS : Version_Set) return Boolean renames Is_In;

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

   package UStrings renames Ada.Strings.Unbounded;
   subtype UString is UStrings.Unbounded_String;
   use all type UString;

   type Version is record
      Major,
      Minor,
      Patch : Point := 0;
      Pre_Release,
      Build : UString := Ada.Strings.Unbounded.Null_Unbounded_String;
   end record;

   function "=" (L, R : Version) return Boolean is
     (L.Major = R.Major and then
      L.Minor = R.Minor and then
      L.Patch = R.Patch and then
      L.Pre_Release = R.Pre_Release);

   function Major (V : Version) return Point is (V.Major);
   function Minor (V : Version) return Point is (V.Minor);
   function Patch (V : Version) return Point is (V.Patch);
   function Pre_Release (V : Version) return String is (UStrings.To_String (V.Pre_Release));
   function Build (V : Version) return String is (UStrings.To_String (V.Build));

   function New_Version (Major : Point;
                         Minor,
                         Patch : Point := 0;
                         Pre_Release,
                         Build : String := "") return Version
   is (Major => Major,
       Minor => Minor,
       Patch => Patch,
       Pre_Release => UStrings.To_Unbounded_String (Pre_Release),
       Build       => UStrings.To_Unbounded_String (Build));

   function Next_Patch (V : Version;
                        Pre_Release,
                        Build : String := "") return Version
   is (New_Version (V.Major,
                    V.Minor,
                    V.Patch + 1,
                    Pre_Release,
                    Build));

   function Next_Minor (V : Version;
                        Pre_Release,
                        Build : String := "") return Version
   is (New_Version (V.Major,
                    V.Minor + 1,
                    0,
                    Pre_Release,
                    Build));

   function Next_Major (V : Version;
                        Pre_Release,
                        Build : String := "") return Version
   is (New_Version (V.Major + 1,
                    0,
                    0,
                    Pre_Release,
                    Build));

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

   function Image (P : Point) return String is
      (Ada.Strings.Fixed.Trim (P'Img, Ada.Strings.Both));

   function Image (V : Version) return Version_String is
     (Image (V.Major) & "." &
      Image (V.Minor) & "." &
      Image (V.Patch) &
      (if V.Pre_Release /= "" then "-" & Ustrings.To_String (V.Pre_Release) else "") &
      (if V.Build /= "" then "+" & Ustrings.To_String (V.Build) else ""));

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

   function Begins_With (S : String; Pattern : String) return Boolean;

   function Begins_With_Relational (S       : String;
                                    Unicode : Boolean := False) return Boolean;
   --  Checks if S starts with a relational operator, optionally in unicode.
   --  Includes tilde and caret.

end Semantic_Versioning;
