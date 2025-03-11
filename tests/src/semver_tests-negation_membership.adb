procedure Semver_Tests.Negation_Membership is
begin
   Assert (X.Is_In (V ("1"), X.Value ("!2")));
   Assert (not X.Is_In (V ("1"), X.Value ("!1")));
   Assert (X.Is_In (V ("1-rc"), X.Value ("!1")));
   Assert (X.Is_In (V ("1.1"), X.Value ("!1")));
   Assert (X.Is_In (V ("1.1"), X.Value ("!~1.0")));
   Assert (not X.Is_In (V ("1.1"), X.Value ("!^1")));
   Assert (X.Is_In (V ("2"), X.Value ("!1|2")));
   Assert (X.Is_In (V ("2"), X.Value ("!1|3")));
   Assert (X.Is_In (V ("2"), X.Value ("1|!3")));
   Assert (not X.Is_In (V ("1"), X.Value ("!(1|3)")));
   Assert (not X.Is_In (V ("3"), X.Value ("!(1|3)")));
end Semver_Tests.Negation_Membership;
