procedure Semver_Tests.Same_Version is
begin
   Assert (V ("1.0.0") = V ("1.0.0"));
   Assert (V ("1.0.0") = V ("1.0.0+buildmetadata"));
   Assert (V ("1.0.0") /= V ("1.0.0-prerelease"));
end Semver_Tests.Same_Version;
