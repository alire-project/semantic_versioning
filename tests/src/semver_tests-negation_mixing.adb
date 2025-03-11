procedure Semver_Tests.Negation_Mixing is
begin
   Assert (X.Value ("!1") = not X.Value ("1"));
   Assert (X.Value ("!1") = not X.Value ("=1"));
   Assert (X.Value ("!=1") = not X.Value ("=1"));
   Assert (X.Value ("!(=1)") = not X.Value ("=1"));
   Assert (X.Value ("!1&2") = (not X.Value ("=1") and X.Value ("=2")));
   Assert (X.Value ("!(1)&2") = (not X.Value ("=1") and X.Value ("=2")));
end Semver_Tests.Negation_Mixing;
