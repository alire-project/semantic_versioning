procedure Semver_Tests.Next_Version is
begin
   Assert (V ("1.1.1") = Next_Patch (V1_1_0));
   Assert (V ("1.2.0") = Next_Minor (V1_1_0));
   Assert (V ("2.0.0") = Next_Major (V1_1_0));
end Semver_Tests.Next_Version;
