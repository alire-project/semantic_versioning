procedure Semver_Tests.With_Unicode is
begin
   Assert (X.Is_In (V ("1.1"), X.Value_U ("≠1")));
   Assert (X.Is_In (V ("1.1"), X.Value_U ("≥1")));
   Assert (X.Is_In (V ("1.1"), X.Value_U ("≤1.1")));
end Semver_Tests.With_Unicode;
