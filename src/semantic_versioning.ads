private with Ada.Strings;
private with Ada.Strings.Fixed;
private with Ada.Strings.Unbounded;

package Semantic_Versioning with Preelaborate is       
   
   type Point is range 0 .. 999_999;
   
   function Image (P : Point) return String;
   
   subtype Version_String is String
     with Dynamic_Predicate => (for all S of Version_String => S /= ' ');
   
   type Version (<>) is private;
   
   --  A version is a major, minor and patch number
   --  Optionally it may include pre-release name and build metadata, e.g.:
   --  1.2.0-alpha+c3423fab   
   
   type Version_Set (<>) is private;      
   
   --  A collection of versions (usually a compatible subset) 
   
   None : constant Version_Set;   
   
   function New_Version (Major : Point;
                         Minor, 
                         Patch : Point := 0;
                         Pre_Release,
                         Build : String := "") return Version;
   -- Refer to http://semver.org/ for the exact meaning of each part.
   -- Only the three numbers are mandatory.
   
   function New_Version (Description : Version_String) return Version;   
   function V           (Description : Version_String) return Version renames New_Version;
   
   function "<" (L, R : Version) return Boolean;    
   -- Refer to http://semver.org/ for the exact ordering. Most notably:
   -- A version with pre-release tag is earlier than its regular version.
   -- Build info is not taken into account to determine ordering.
   
   function Major (V : Version) return Point;
   function Minor (V : Version) return Point;
   function Patch (V : Version) return Point;
   function Pre_Release (V : Version) return String;
   function Build (V : Version) return String;
   
   function Image (V : Version) return Version_String;
   
   function Next_Patch (V : Version;
                        Pre_Release, 
                        Build : String := "") return Version;
   
   function Next_Minor (V : Version;
                        Pre_Release, 
                        Build : String := "") return Version;
   
   function Next_Major (V : Version;
                        Pre_Release, 
                        Build : String := "") return Version;
   
   function At_Least_Within_Major (V : Version) return Version_Set;
   
   function At_Least  (V : Version) return Version_Set;
   function At_Most   (V : Version) return Version_Set;
   function Less_Than (V : Version) return Version_Set;
   function More_Than (V : Version) return Version_Set;
   function Exactly   (V : Version) return Version_Set;
   function Except    (V : Version) return Version_Set;
   
   function "and" (VS1, VS2 : Version_Set) return Version_Set;
   
   function Is_In     (V : Version; VS : Version_Set) return Boolean;
   function Satisfies (V : Version; VS : Version_Set) return Boolean renames Is_In;
   
private
   
   package UStrings renames Ada.Strings.Unbounded;
   subtype UString is UStrings.Unbounded_String;
   
   type Version is record
      Major,
      Minor,
      Patch : Point := 0;
      Pre_Release,
      Build : UString := Ada.Strings.Unbounded.Null_Unbounded_String;
   end record;
   
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
   
   type Conditions is (At_Least, At_Most, Exactly, Except);
   
   type Restriction is record
      Condition  : Conditions;
      On_Version : Version;
   end record;           
   
   function Satisfies (V : Version; R : Restriction) return Boolean;
   
   type Version_Set is array (Positive range <>) of Restriction;      
   
   function At_Least_Within_Major (V : Version) return Version_Set is
      (At_Least (V) and Less_Than (Next_Major (V)));
   
   function At_Least  (V : Version) return Version_Set is (1 => (At_Least, V));   
   function At_Most   (V : Version) return Version_Set is (1 => (At_Most, V));
   function Less_Than (V : Version) return Version_Set is (At_Most (V) and Except (V));   
   function More_Than (V : Version) return Version_Set is (At_Least (V) and Except (V));
   function Exactly   (V : Version) return Version_Set is (1 => (Exactly, V));
   function Except    (V : Version) return Version_Set is (1 => (Except, V));
   
   None : constant Version_Set := (1 .. 0 => <>);
   
   function "and" (VS1, VS2 : Version_Set) return Version_Set is (VS1 & VS2);
   
   function Image (P : Point) return String is
      (Ada.Strings.Fixed.Trim (P'Image, Ada.Strings.Both));
   
   use type UString;
   
   function Image (V : Version) return Version_String is
     (Image (V.Major) & "." &
      Image (V.Minor) & "." &
      Image (V.Patch) &
      (if V.Pre_Release /= "" then "-" & Ustrings.To_String (V.Pre_Release) else "") &
      (if V.Build /= "" then "+" & Ustrings.To_String (V.Build) else ""));
   
end Semantic_Versioning;
