procedure Semver_Tests.Compare_Prerelease is
begin
   Assert (V ("1.0.0-alpha")      < V ("1.0.0-alpha.1"));
   Assert (V ("1.0.0-alpha.1")    < V ("1.0.0-alpha.beta"));
   Assert (V ("1.0.0-alpha.beta") < V ("1.0.0-beta"));
   Assert (V ("1.0.0-beta")       < V ("1.0.0-beta.2"));
   Assert (V ("1.0.0-beta.2")     < V ("1.0.0-beta.11"));
   Assert (V ("1.0.0-beta.11")    < V ("1.0.0-rc.1"));
   Assert (V ("1.0.0-rc.1")       < V ("1.0.0"));
end Semver_Tests.Compare_Prerelease;
