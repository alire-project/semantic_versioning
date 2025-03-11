procedure Semver_Tests.Basic_Expressions is
begin
   Assert (B.Is_In (V ("1.0"), B.Value ("^1 & <2")));
   Assert (not B.Is_In (V ("1.1"), B.Value ("^1 & <2 & /=1.1")));
   Assert (not B.Parse ("/= 1").Valid); -- Because space
   Assert (not B.Parse ("<1|>2").Valid); -- Because |
   Assert (not B.Parse ("<1 | >2").Valid); -- Because |
   Assert (not B.Parse ("&1").Valid);
   Assert (not B.Parse ("&").Valid);
   Assert (B.Value ("*").Image = B.Any.Image);
   Assert (B.Value ("*").Image = B.Value ("any").Image);
end Semver_Tests.Basic_Expressions;
