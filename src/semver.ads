private with Ada.Strings.Unbounded;

package Semver with Preelaborate is       
   
   type Point is range 0 .. 999_999;
   
   type Version (<>) is private;
   
   --  A version is a major, minor and patch number
   --  Optionally it may include pre-release name and build metadata, e.g.:
   --  1.2.0-alpha+c3423fab   
   
   type Version_Set (<>) is private;      
   
   --  A collection of dependencies 
   
   None : constant Version_Set;
   
   function New_Version (Major : Point;
                         Minor, 
                         Patch : Point := 0;
                         Pre_Release,
                         Build : String := "") return Version;
   -- Refer to http://semver.org/ for the exact meaning of each part.
   -- Only the three numbers are mandatory.
   
   function New_Version (Description : String) return Version;   
   function "+"         (Description : String) return Version renames New_Version;
   
   function "<" (L, R : Version) return Boolean;         
   
   function At_Least (V : Version) return Version_Set;
   function At_Most  (V : Version) return Version_Set;
   function Less_Than (V : Version) return Version_Set;
   function Exactly  (V : Version) return Version_Set;
   function Except   (V : Version) return Version_Set;
   
   function "and" (L, R : Version_Set) return Version_Set;
   
   function Is_In (V : Version; R : Version_Set) return Boolean;
   
private
   
   subtype UString is Ada.Strings.Unbounded.Unbounded_String;
   
   type Version is record
      Major,
      Minor,
      Patch : Natural;
      Pre_Release,
      Build : UString;
   end record;
   
   type Conditions is (At_Least, At_Most, Exactly, Except);
   
   type Restriction is record
      Condition  : Conditions;
      On_Version : Version;
   end record;     
   
   type Version_Set is array (Positive range <>) of Restriction;      
   
   None : constant Version_Set := (1 .. 0 => <>);
   
   function "and" (L, R : Version_Set) return Version_Set is (L & R);
   
   function Less_Than (V : Version) return Version_Set is (At_Most(V) and Except(V));

end Semver;
