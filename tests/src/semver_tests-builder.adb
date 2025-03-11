procedure Semver_Tests.Builder is
begin
   Assert (New_Version (1, 2, 3) = New_Version ("1.2.3"));
end Semver_Tests.Builder;
