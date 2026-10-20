procedure Semver_Tests.Default_Output is
   --  Images without explicit options keep the backwards-compatible output:
   --  ASCII operators and an explicit "=", even though parsing accepts the
   --  Unicode and implicit forms by default (see Basic.Output_Options).

   V : constant Version := New_Version ("1.2.0");
begin
   Assert (B.Image (B.Exactly (V)) = "=1.2.0");
   Assert (B.Image (B.Except (V)) = "/=1.2.0");
   Assert (B.Image (B.At_Least (V)) = ">=1.2.0");
   Assert (B.Image_Abbreviated (B.Exactly (V)) = "=1.2.0");

   --  Extended sets built from basic ones, as done when combining a pinned
   --  version with a dependency.

   Assert (X.To_Extended (B.Exactly (V)).Image = "=1.2.0");
   Assert (X.Image (X.To_Extended (B.Exactly (V))
                    and X.To_Extended (B.Within_Major (V)))
           = "(=1.2.0) & (^1.2.0)");
end Semver_Tests.Default_Output;
