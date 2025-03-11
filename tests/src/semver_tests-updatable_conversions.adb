procedure Semver_Tests.Updatable_Conversions is
begin
   Assert (Updatable (V ("0.0.2")) = X.Value ("~0.0.2"));
   Assert (Updatable (V ("0.3.2")) = X.Value ("~0.3.2"));
   Assert (Updatable (V ("3.0.2")) = X.Value ("^3.0.2"));
end Semver_Tests.Updatable_Conversions;
