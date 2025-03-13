procedure Semver_Tests.Extended_Operators is
begin
   Assert (((X.Value (">=1") and X.Value ("<2")) or X.Value ("=3")) =
           X.Value ("(>=1 & <2)|=3"));
end Semver_Tests.Extended_Operators;
