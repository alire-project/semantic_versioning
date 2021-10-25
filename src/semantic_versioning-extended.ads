with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

with Semantic_Versioning.Basic;

package Semantic_Versioning.Extended with Preelaborate is

   type Version_Set is tagged private;

   function Any return Version_Set;

   function Is_Any (VS : Version_Set) return Boolean;
   --  Same as VS = Any

   function "=" (L, R : Version_Set) return Boolean;

   type Result (Valid  : Boolean;
                Length : Natural) is record
      case Valid is
         when True =>
            Set : Version_Set;
         when False =>
            Error : String (1 .. Length);
      end case;
   end record;

   function "and" (L, R : Version_Set) return Version_Set;
   --  Creates a new tree that is Simplify ((L) & (R)).

   function "or" (L, R : Version_Set) return Version_Set;
   --  Creates a new tree that is Simplify ((L) | (R)).

   function "not" (VS : Version_Set) return Version_Set;
   --  Creates a new tree that is Simplify (~VS)

   function Simplify (VS : Version_Set) return Version_Set;
   --  Apply trivial and/or simplifications to the set: "* & =1.0" --> "=1.0",
   --  "* | =1.0" --> "*", "=1.0 & =1.0" --> "=1.0"... Won't be clever enough
   --  to simplify "^1.0 & ^1.1" --> "^1.1" and similar.

   function Is_In (V : Version; VS : Version_Set) return Boolean;

   function Is_Single_Version (VS : Version_Set) return Boolean;
   --  Says if this VS encapsulates a single "=x.y.z" condition

   function Contains (VS : Version_Set; V : Version) return Boolean is
      (Is_In (V, VS));

   function To_Extended (BVS : Basic.Version_Set)
                         return Version_Set;

   function Parse (Str     : String;
                   Relaxed : Boolean := False;
                   Unicode : Boolean := True) return Result;
   --  Parse a string and return an extended version set or an error message
   --  pinpointing the error.
   --  If Unicode, plain and unicode sequences are both accepted.
   --  Relaxed is passed to Semantic_Versioning.To_Set for set conditions.

   function Value (Str     : String;
                   Relaxed : Boolean := False;
                   Unicode : Boolean := True) return Version_Set;
   --  This version will raise Malformed_Input with the corresponding error as
   --  message, instead of returning a Result.

   function Image (VS : Version_Set) return String;
   --  Original image, as given to Value

   function Synthetic_Image (VS      : Version_Set;
                             Unicode : Boolean := False) return String;
   --  Reconstructed normalized image

private

   use Ada.Strings.Unbounded;

   package Semver renames Semantic_Versioning;

   --  A version set is a binary tree of and/or lists, with leaves being
   --  basic version sets.

   type Kinds is (Anded, Ored, Negated, Leaf);

   type Any_Node (Kind : Kinds := Leaf) is record
      case Kind is
         when Leaf =>
            VS : Basic.Version_Set;
         when others =>
            null;
      end case;
   end record;

   package Trees is new Ada.Containers.Multiway_Trees (Any_Node);

   type Version_Set is tagged record
      Set   : Trees.Tree;
      Image : Unbounded_String;
   end record;

   --  For internal use:

   ----------------------
   -- New_Valid_Result --
   ----------------------

   function New_Valid_Result (VS : Version_Set) return Result is
     (Valid => True, Length => 0, Set => VS);

   Empty_Set : constant Version_Set := (others => <>);

end Semantic_Versioning.Extended;
