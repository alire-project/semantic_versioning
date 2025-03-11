procedure Semver_Tests.Ordering is
begin
   Assert (V1_0_0_Alpha < V1_0_0);
   Assert (not (V1_0_0 < V1_0_0_Alpha));

   Assert (V1_0_0_Alpha < V1_1_0);
   Assert (V1_0_0_Alpha < V1_Beta);

   Assert (V1_0_0 < V1_1_0);
   Assert (V1_1_0 < V1_1_1);

   Assert (not (V1_1_0 < V1_1_0));

   Assert (V1_0_0 < Next_Patch (V1_1_0));
   Assert (V1_0_0 < Next_Minor (V1_1_0));
   Assert (V1_0_0 < Next_Major (V1_1_0));

   Assert (V ("1.0") < V ("1.1-rc1"));
end Semver_Tests.Ordering;
