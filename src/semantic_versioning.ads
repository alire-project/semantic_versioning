private with Ada.Strings;
private with Ada.Strings.Fixed;
private with Ada.Strings.Unbounded;

limited with Semantic_Versioning.Basic;
limited with Semantic_Versioning.Extended;

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

   type Version is tagged private;
   --  A version is a major, minor and patch number
   --  Optionally it may include pre-release name and build metadata, e.g.:
   --  1.2.0-alpha+c3423fab

   --  A collection of versions (usually a compatible subset)

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

   --  For convenience, the following conversion functions are made available
   --  in this top-level package:

   function To_Basic    (V  : Version) return Basic.Version_Set;
   function To_Extended (V  : Version) return Extended.Version_Set;
   function To_Extended (VS : Basic.Version_Set) return Extended.Version_Set;

   function Updatable (V : Version) return Extended.Version_Set;
   --  Given a version, return its appropriate updatable set. E.g.:
   --  0.0.1 --> ~0.0.1
   --  0.1.1 --> ~0.1.1
   --  1.0.1 --> ^1.0.1

private

   package UStrings renames Ada.Strings.Unbounded;
   subtype UString is UStrings.Unbounded_String;
   use all type UString;

   type Version is tagged record
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

   function Image (P : Point) return String is
      (Ada.Strings.Fixed.Trim (P'Img, Ada.Strings.Both));

   function Image (V : Version) return Version_String is
     (Image (V.Major) & "." &
      Image (V.Minor) & "." &
      Image (V.Patch) &
      (if V.Pre_Release /= "" then "-" & Ustrings.To_String (V.Pre_Release) else "") &
      (if V.Build /= "" then "+" & Ustrings.To_String (V.Build) else ""));

   function Begins_With (S : String; Pattern : String) return Boolean;

   function Begins_With_Relational (S       : String;
                                    Unicode : Boolean := False) return Boolean;
   --  Checks if S starts with a relational operator, optionally in unicode.
   --  Includes tilde and caret.

end Semantic_Versioning;
