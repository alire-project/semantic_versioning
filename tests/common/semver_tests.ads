pragma Warnings (GNAT, Off, "no entities of * are referenced");
pragma Warnings (GNAT, Off, "use clause for * has no effect");

with Ada.Assertions; use Ada.Assertions;

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;
use Semantic_Versioning;

package Semver_Tests is
   V1_0_0 : constant Version := New_Version (1);
   V1_1_0 : constant Version := New_Version (1, 1);
   V1_1_1 : constant Version := New_Version (1, 1, 1);

   V1_0_0_Alpha_Img : constant String := "1.0.0-alpha";

   V1_0_0_Alpha : constant Version := New_Version (V1_0_0_Alpha_Img);
   V1_Beta      : constant Version := New_Version ("1-beta+6699dd338e");

   use Basic;
   use all type Extended.Version_Set;
   package B renames Basic;
   package X renames Extended;
end Semver_Tests;
