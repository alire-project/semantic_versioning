with GNAT.IO; use GNAT.IO;

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

procedure Semantic_Versioning.Demo is
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
begin
   -- Builder
   pragma Assert (New_Version (1, 2, 3) = New_Version ("1.2.3"));

   -- Restrictions
   pragma Assert (Satisfies (V1_0_0, At_Least (V1_0_0)));
   pragma Assert (Satisfies (V1_0_0, At_Most  (V1_0_0)));
   pragma Assert (Satisfies (V1_0_0, Exactly  (V1_0_0)));

   pragma Assert (not Satisfies (V1_0_0, Less_Than (V1_0_0)));
   pragma Assert (not Satisfies (V1_0_0, Except    (V1_0_0)));

   pragma Assert (Satisfies (V1_0_0, Less_Than (V1_1_0)));
   pragma Assert (Satisfies (V1_0_0, Except    (V1_1_0)));

   pragma Assert (Satisfies (V1_1_0, At_Least (V1_0_0) and At_Most (V1_1_1)));

   pragma Assert (Satisfies (V ("1.1.9"),     Within_Major (V ("1.1.0"))));
   pragma Assert (not Satisfies (V ("1.1.9"), Within_Major (V ("2.0.0-alpha"))));
   pragma Assert (not Satisfies (V ("2.0.0-alpha"), Within_Major (V ("1.0.0"))));

   pragma Assert (Satisfies (V ("1.1.9"),     Within_Minor (V ("1.1.0"))));
   pragma Assert (not Satisfies (V ("1.1.9"), Within_Minor (V ("1.2.0-alpha"))));
   pragma Assert (not Satisfies (V ("1.2.0-alpha"), Within_Minor (V ("1.1.0"))));

   pragma Assert (    X.Is_In (V ("1.1-1"), X.Value ("^1")));
   pragma Assert (not X.Is_In (V ("1.1-1"), X.Value ("^1.1")));

   -- Reconstruction
   pragma Assert (Image (V1_0_0_Alpha) = V1_0_0_Alpha_Img);

   -- Ordering
   pragma Assert (V1_0_0_Alpha < V1_0_0);
   pragma Assert (not (V1_0_0 < V1_0_0_Alpha));

   pragma Assert (V1_0_0_Alpha < V1_1_0);
   pragma Assert (V1_0_0_Alpha < V1_Beta);

   pragma Assert (V1_0_0 < V1_1_0);
   pragma Assert (V1_1_0 < V1_1_1);

   pragma Assert (not (V1_1_0 < V1_1_0));

   pragma Assert (V1_0_0 < Next_Patch (V1_1_0));
   pragma Assert (V1_0_0 < Next_Minor (V1_1_0));
   pragma Assert (V1_0_0 < Next_Major (V1_1_0));

   -- Build info is not taken into account:
   pragma Assert (not (V ("1.0.0+build0") < V ("1.0.0+build1")));
   pragma Assert (not (V ("1.0.0+build1") < V ("1.0.0+build0")));

   -- Field comparisons in the Pre-release part (taken from semver.org):
   pragma Assert (V ("1.0.0-alpha")      < V ("1.0.0-alpha.1"));
   pragma Assert (V ("1.0.0-alpha.1")    < V ("1.0.0-alpha.beta"));
   pragma Assert (V ("1.0.0-alpha.beta") < V ("1.0.0-beta"));
   pragma Assert (V ("1.0.0-beta")       < V ("1.0.0-beta.2"));
   pragma Assert (V ("1.0.0-beta.2")     < V ("1.0.0-beta.11"));
   pragma Assert (V ("1.0.0-beta.11")    < V ("1.0.0-rc.1"));
   pragma Assert (V ("1.0.0-rc.1")       < V ("1.0.0"));

   -- Other ordering checks
   pragma Assert (V ("1.0")              < V ("1.1-rc1"));

   -- Next-ing
   pragma Assert (V ("1.1.1") = Next_Patch (V1_1_0));
   pragma Assert (V ("1.2.0") = Next_Minor (V1_1_0));
   pragma Assert (V ("2.0.0") = Next_Major (V1_1_0));

   -- Same version
   pragma Assert (V ("1.0.0") = V ("1.0.0"));
   pragma Assert (V ("1.0.0") = V ("1.0.0+buildmetadata"));
   pragma Assert (V ("1.0.0") /= V ("1.0.0-prerelease"));

   -- To Set from string
   pragma Assert (Basic.Any = To_Set ("any"));
   pragma Assert (Basic.Any = To_Set ("*"));

   pragma Assert (Exactly (V1_0_0) = To_Set ("1.0.0"));
   pragma Assert (Exactly (V1_0_0) = To_Set ("=1.0.0"));

   pragma Assert (Except  (V1_0_0) = To_Set ("/=1.0.0"));
   pragma Assert (Except  (V1_0_0) = To_Set ("≠1.0.0"));

   pragma Assert (At_Least (V1_0_0) = To_Set (">=1.0.0"));
   pragma Assert (At_Least (V1_0_0) = To_Set ("≥1.0.0"));

   pragma Assert (At_Most (V1_0_0) = To_Set ("<=1.0.0"));
   pragma Assert (At_Most (V1_0_0) = To_Set ("≤1.0.0"));

   pragma Assert (More_Than (V1_0_0) = To_Set (">1.0.0"));
   pragma Assert (Less_Than (V1_0_0) = To_Set ("<1.0.0"));

   -- Basic expressions
   pragma Assert (B.Is_In (V ("1.0"), B.Value ("^1 & <2")));
   pragma Assert (not B.Is_In (V ("1.1"), B.Value ("^1 & <2 & /=1.1")));
   pragma Assert (not B.Parse ("/= 1").Valid); -- Because space
   pragma Assert (not B.Parse ("<1|>2").Valid); -- Because |
   pragma Assert (not B.Parse ("<1 | >2").Valid); -- Because |
   pragma Assert (not B.Parse ("&1").Valid);
   pragma Assert (not B.Parse ("&").Valid);
   pragma Assert (B.Value ("*").Image = B.Any.Image);
   pragma Assert (B.Value ("*").Image = B.Value ("any").Image);

   --  Extended expressions
   pragma Assert (X.Is_In (V ("1.0"), X.Value ("1")));
   pragma Assert (X.Is_In (V ("1.0"), X.Value ("2|1")));
   pragma Assert (X.Is_In (V ("1.1"), X.Value ("2|^1")));
   pragma Assert (not X.Is_In (V ("1.1"), X.Value ("^1&/=1.1")));
   pragma Assert (X.Is_In (V ("1.2"), X.Value ("^2|/=1.1")));
   pragma Assert (X.Is_In (V ("1"), X.Value ("((4-rc))|(^3&~3)|^2+build|=1")));
   pragma Assert (not X.Parse ("(").Valid);
   pragma Assert (not X.Parse ("()").Valid);
   pragma Assert (not X.Parse ("(1").Valid);
   pragma Assert (not X.Parse ("1&2|3").Valid);
   pragma Assert (not X.Parse ("&1").Valid);
   pragma Assert (not X.Parse ("&").Valid);
   pragma Assert (X.Parse ("1&(2|3)").Valid);
   pragma Assert (X.Parse ("((1&(2|3)))").Valid);
   pragma Assert (X.Value ("*").Image = X.Any.Image);
   pragma Assert (X.Value ("*").Image = X.Value ("any").Image);

   --  Simplifications within extended expressions
   pragma Assert (X.Value ("1") = (X.Value ("1") and X.Value ("1")));
   pragma Assert (X.Value ("*") = (X.Value ("*") or X.Value ("1.0")));
   pragma Assert (X.Value ("1.0") = (X.Value ("*") and X.Value ("1.0")));
   pragma Assert ((X.Value ("1") and
                    (X.Value ("2") and X.Value ("1"))) =
                  (X.Value ("2") and X.Value ("1")));
   pragma Assert ((X.Value ("1") or
                    (X.Value ("2") or
                         (X.Value ("3") or X.Value ("1")))) =
                  (X.Value ("2") or
                    (X.Value ("3") or X.Value ("1"))));

   --  X + Unicode
   pragma Assert (X.Is_In (V ("1.1"), X.Value ("≠1")));
   pragma Assert (X.Is_In (V ("1.1"), X.Value ("≥1")));
   pragma Assert (X.Is_In (V ("1.1"), X.Value ("≤1.1")));

   --  Extended operators
   pragma Assert (((X.Value (">=1") and X.Value ("<2")) or X.Value ("=3")) =
                    X.Value ("(>=1 & <2)|=3"));

   --  Updatable conversions
   pragma Assert (Updatable (V ("0.0.2")) = X.Value ("~0.0.2"));
   pragma Assert (Updatable (V ("0.3.2")) = X.Value ("~0.3.2"));
   pragma Assert (Updatable (V ("3.0.2")) = X.Value ("^3.0.2"));

   --  Negation
   pragma Assert (not X.Parse ("!!1").Valid); -- no double negation
   pragma Assert (not X.Parse ("!(!1)").Valid); -- no double negation
   pragma Assert (not X.Parse ("!").Valid); -- no dangling
   pragma Assert (not X.Parse ("3|!").Valid); -- no dangling
   pragma Assert (X.Parse ("!(!1 | 2)").Valid); -- Allowed b.c. of nested EVS
   pragma Assert (X.Value ("!1&2").Synthetic_Image   = "!(=1.0.0)&=2.0.0"); -- Respect precedence
   pragma Assert (X.Value ("!(1&2)").Synthetic_Image = "!(=1.0.0&=2.0.0)"); -- Respect precedence
   pragma Assert (X.Value ("!^1").Synthetic_Image = "!(^1.0.0)");
   pragma Assert (X.Value ("!^1").Synthetic_Image = X.Value ("!(^1)").Synthetic_Image); -- Equivalent

   --  Negation operator and mixing
   pragma Assert (X.Value ("!1") = not X.Value ("1"));
   pragma Assert (X.Value ("!1") = not X.Value ("=1"));
   pragma Assert (X.Value ("!=1") = not X.Value ("=1"));
   pragma Assert (X.Value ("!(=1)") = not X.Value ("=1"));
   pragma Assert (X.Value ("!1&2") = (not X.Value ("=1") and X.Value ("=2")));
   pragma Assert (X.Value ("!(1)&2") = (not X.Value ("=1") and X.Value ("=2")));

   --  Negation membership tests
   pragma Assert (X.Is_In (V ("1"), X.Value ("!2")));
   pragma Assert (not X.Is_In (V ("1"), X.Value ("!1")));
   pragma Assert (X.Is_In (V ("1-rc"), X.Value ("!1")));
   pragma Assert (X.Is_In (V ("1.1"), X.Value ("!1")));
   pragma Assert (X.Is_In (V ("1.1"), X.Value ("!~1.0")));
   pragma Assert (not X.Is_In (V ("1.1"), X.Value ("!^1")));
   pragma Assert (X.Is_In (V ("2"), X.Value ("!1|2")));
   pragma Assert (X.Is_In (V ("2"), X.Value ("!1|3")));
   pragma Assert (X.Is_In (V ("2"), X.Value ("1|!3")));
   pragma Assert (not X.Is_In (V ("1"), X.Value ("!(1|3)")));
   pragma Assert (not X.Is_In (V ("3"), X.Value ("!(1|3)")));

   Put_Line ("OK");
end Semantic_Versioning.Demo;
