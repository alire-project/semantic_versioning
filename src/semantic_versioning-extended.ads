with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

package Semantic_Versioning.Extended with Preelaborate is

   type Version_Set is tagged private;

   type Result (Valid : Boolean;
                Len   : Natural) is record
      case Valid is
         when True =>
            VS : Version_Set;
         when False =>
            Error : String (1 .. Len);
      end case;
   end record;

   function Is_In (V : Version; VS : Version_Set) return Boolean;

   function Contains (VS : Version_Set; V : Version) return Boolean is
      (Is_In (V, VS));

   function Value (Str     : String;
                   Relaxed : Boolean := False;
                   Unicode : Boolean := True) return Result;
   --  Parse a string and return an extended version set or an error message
   --  pinpointing the error.
   --  If Unicode, plain and unicode sequences are both accepted.
   --  Relaxed is passed to Semantic_Versioning.To_Set for set conditions.

   function Image (VS : Version_Set) return String;
   --  Original image, as given to Value

   function Synthetic_Image (VS : Version_Set) return String;
   --  Reconstructed normalized image

private

   use Ada.Strings.Unbounded;

   package Semver renames Semantic_Versioning;

   --  A version set is a binary tree of and/or lists, with leaves being
   --  basic version sets.

   type Kinds is (Anded, Ored, Leaf);

   type Any_Node (Kind : Kinds := Leaf) is record
      case Kind is
         when Leaf =>
            VS : Semantic_Versioning.Version_Set;
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

   function New_Valid_Result (VS : Version_Set) return Result is
     (Valid => True, Len => 0, VS => VS);

   Empty_Set : constant Version_Set := (others => <>);

end Semantic_Versioning.Extended;
