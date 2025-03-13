procedure Semver_Tests.Negation is
begin
   Assert (not X.Parse ("!!1").Valid); --  no double negation
   Assert (not X.Parse ("!(!1)").Valid); --  no double negation
   Assert (not X.Parse ("!").Valid); --  no dangling
   Assert (not X.Parse ("3|!").Valid); --  no dangling
   Assert (X.Parse ("!(!1 | 2)").Valid); --  Allowed b.c. of nested EVS
   Assert (X.Value ("!1&2").Synthetic_Image = "!(=1.0.0)&=2.0.0");
   --  Respect precedence

   Assert (X.Value ("!(1&2)").Synthetic_Image = "!(=1.0.0&=2.0.0)");
   --  Respect precedence

   Assert (X.Value ("!^1").Synthetic_Image = "!(^1.0.0)");
   Assert
     (X.Value ("!^1").Synthetic_Image = X.Value ("!(^1)").Synthetic_Image);
   --  Equivalent
end Semver_Tests.Negation;
