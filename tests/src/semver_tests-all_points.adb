procedure Semver_Tests.All_Points is
   --  Has_All_Points tells whether major, minor and patch are all given, which
   --  semantic versioning requires but the parsers do not.
begin
   Assert (Has_All_Points ("1.0.0"));
   Assert (Has_All_Points ("10.20.30"));
   Assert (Has_All_Points ("1.0.0-rc.1"));
   Assert (Has_All_Points ("1.0.0+exp.sha"));
   Assert (Has_All_Points ("1.0.0-rc.1.2+build.3.4"));

   Assert (not Has_All_Points ("1"));
   Assert (not Has_All_Points ("1.0"));
   Assert (not Has_All_Points ("1.0-rc.1.2"));
   Assert (not Has_All_Points ("1.0+build.3.4"));
   Assert (not Has_All_Points ("1.0.0.0"));

   --  The parsers accept the missing points, defaulting them to 0

   Assert (Parse ("1.0") = V ("1.0.0"));
end Semver_Tests.All_Points;
