procedure Semver_Tests.Restrictions is
begin
   Assert (Satisfies (V1_0_0, At_Least (V1_0_0)));
   Assert (Satisfies (V1_0_0, At_Most (V1_0_0)));
   Assert (Satisfies (V1_0_0, Exactly (V1_0_0)));

   Assert (not Satisfies (V1_0_0, Less_Than (V1_0_0)));
   Assert (not Satisfies (V1_0_0, Except (V1_0_0)));

   Assert (Satisfies (V1_0_0, Less_Than (V1_1_0)));
   Assert (Satisfies (V1_0_0, Except (V1_1_0)));

   Assert (Satisfies (V1_1_0, At_Least (V1_0_0) and At_Most (V1_1_1)));

   Assert (Satisfies (V ("1.1.9"), Within_Major (V ("1.1.0"))));
   Assert (not Satisfies (V ("1.1.9"), Within_Major (V ("2.0.0-alpha"))));
   Assert (not Satisfies (V ("2.0.0-alpha"), Within_Major (V ("1.0.0"))));

   Assert (Satisfies (V ("1.1.9"), Within_Minor (V ("1.1.0"))));
   Assert (not Satisfies (V ("1.1.9"), Within_Minor (V ("1.2.0-alpha"))));
   Assert (not Satisfies (V ("1.2.0-alpha"), Within_Minor (V ("1.1.0"))));

   Assert (X.Is_In (V ("1.1-1"), X.Value ("^1")));
   Assert (not X.Is_In (V ("1.1-1"), X.Value ("^1.1")));
end Semver_Tests.Restrictions;
