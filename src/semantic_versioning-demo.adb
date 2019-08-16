procedure Semantic_Versioning.Demo is
   V1_0_0 : constant Version := New_Version (1);
   V1_1_0 : constant Version := New_Version (1, 1);
   V1_1_1 : constant Version := New_Version (1, 1, 1);

   V1_0_0_Alpha_Img : constant String := "1.0.0-alpha";

   V1_0_0_Alpha : constant Version := New_Version (V1_0_0_Alpha_Img);
   V1_Beta      : constant Version := New_Version("1-beta+6699dd338e");
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

   -- Next-ing
   pragma Assert (V ("1.1.1") = Next_Patch (V1_1_0));
   pragma Assert (V ("1.2.0") = Next_Minor (V1_1_0));
   pragma Assert (V ("2.0.0") = Next_Major (V1_1_0));

   -- Same version
   pragma Assert (V ("1.0.0") = V ("1.0.0"));
   pragma Assert (V ("1.0.0") = V ("1.0.0+buildmetadata"));
   pragma Assert (V ("1.0.0") /= V ("1.0.0-prerelease"));

   -- To Set from string
   pragma Assert (Any = To_Set ("any"));
   pragma Assert (Any = To_Set ("*"));

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
end Semantic_Versioning.Demo;
