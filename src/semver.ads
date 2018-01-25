private with Ada.Strings.Unbounded;

package Semver with Preelaborate is       
   
   type Point is range 0 .. 999_999;
   
   type Version (<>) is private;
   
   --  A version is a major, minor and patch number
   --  Optionally it may include pre-release name and build metadata, e.g.:
   --  1.2.0-alpha+c3423fab   
   
   type Requisites (<>) is private;      
   
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
   
   function At_Least (V : Version) return Requisites;
   function At_Most  (V : Version) return Requisites;
   function Less_Than (V : Version) return Requisites;
   function Exactly  (V : Version) return Requisites;
   function Except   (V : Version) return Requisites;
   
   function "and" (L, R : Requisites) return Requisites;
   
   function Satisfies (V : Version; R : Requisites) return Boolean;
   
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
   
   type Requisite is record
      Condition : Conditions;
      Ver       : Version;
   end record;
   
   function Satisfies (V : Version; R : Requisite) return Boolean;
   
   type Requisites is array (Positive range <>) of Requisite;      
   
   function "and" (L, R : Requisites) return Requisites is (L & R);
   
   function Less_Than (V : Version) return Requisites is (At_Most(V) and Except(V));

end Semver;
