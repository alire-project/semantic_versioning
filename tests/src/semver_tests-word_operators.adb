procedure Semver_Tests.Word_Operators is
begin
   Assert (X.Parse ("1 and 2").Valid);
   Assert (X.Parse ("1 or 2").Valid);
   Assert (X.Parse ("not 1").Valid);
   Assert (X.Parse ("NoT 1").Valid);
   Assert (X.Parse ("1 and (2 or 3)").Valid);
   Assert (X.Value ("not 1 and 2").Synthetic_Image = "!(=1.0.0)&=2.0.0");
   Assert (X.Value ("not (1 or 2)").Synthetic_Image = "!(=1.0.0|=2.0.0)");

   Assert (not X.Parse ("and 1").Valid);
   Assert (not X.Parse ("1 and").Valid);
   Assert (not X.Parse ("1 or").Valid);
   Assert (not X.Parse ("not").Valid);
   Assert (not X.Parse ("1 or not").Valid);
   Assert (not X.Parse ("1 and or 2").Valid);
   Assert (not X.Parse ("1 and 2 or 3").Valid);
end Semver_Tests.Word_Operators;
