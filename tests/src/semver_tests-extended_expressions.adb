procedure Semver_Tests.Extended_Expressions is
begin
   Assert (X.Is_In (V ("1.0"), X.Value ("1")));
   Assert (X.Is_In (V ("1.0"), X.Value ("2|1")));
   Assert (X.Is_In (V ("1.1"), X.Value ("2|^1")));
   Assert (not X.Is_In (V ("1.1"), X.Value ("^1&/=1.1")));
   Assert (X.Is_In (V ("1.2"), X.Value ("^2|/=1.1")));
   Assert (X.Is_In (V ("1"), X.Value ("((4-rc))|(^3&~3)|^2+build|=1")));
   Assert (not X.Parse ("(").Valid);
   Assert (not X.Parse ("()").Valid);
   Assert (not X.Parse ("(1").Valid);
   Assert (not X.Parse ("1&2|3").Valid);
   Assert (not X.Parse ("&1").Valid);
   Assert (not X.Parse ("&").Valid);
   Assert (X.Parse ("1&(2|3)").Valid);
   Assert (X.Parse ("((1&(2|3)))").Valid);
   Assert (X.Value ("*").Image = X.Any.Image);
   Assert (X.Value ("*").Image = X.Value ("any").Image);
   Assert (X.Is_In (V ("1.1"), X.Value ("^1 and <2")));
   Assert (X.Is_In (V ("2"), X.Value ("not =1 or =2"))); -- equivalent to "(!=1)|(=3)"
   Assert (not X.Is_In (V ("1"), X.Value ("not =1 and =2"))); -- equivalent to "(!=1)&(=3)"
end Semver_Tests.Extended_Expressions;
