procedure Semver_Tests.Ignore_Buildinfo is
begin
   Assert (not (V ("1.0.0+build0") < V ("1.0.0+build1")));
   Assert (not (V ("1.0.0+build1") < V ("1.0.0+build0")));
end Semver_Tests.Ignore_Buildinfo;
