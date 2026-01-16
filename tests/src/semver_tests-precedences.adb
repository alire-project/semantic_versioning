procedure Semver_Tests.Precedences is
begin
   Assert (X.Value ("not 1 and 2").Synthetic_Image = "!(=1.0.0)&=2.0.0");
   Assert (X.Value ("not (1 and 2)").Synthetic_Image = "!(=1.0.0&=2.0.0)");
   Assert (X.Value ("not 1 or 2").Synthetic_Image = "!(=1.0.0)|=2.0.0");
   Assert (X.Value ("not (1 or 2)").Synthetic_Image = "!(=1.0.0|=2.0.0)");

   --  Multiple and/or with not
   Assert (X.Value ("not 1 and 2 and 3").Synthetic_Image =
             "!(=1.0.0)&=2.0.0&=3.0.0");
   Assert (X.Value ("not 1 or 2 or 3").Synthetic_Image =
             "!(=1.0.0)|=2.0.0|=3.0.0");
   Assert (X.Value ("not 1 and (2 or 3)").Synthetic_Image =
             "!(=1.0.0)&(=2.0.0|=3.0.0)");
   Assert (X.Value ("(not 1 and 2) or 3").Synthetic_Image =
             "(!(=1.0.0)&=2.0.0)|=3.0.0");
   Assert (X.Value ("not (1 and 2) or 3").Synthetic_Image =
             "!(=1.0.0&=2.0.0)|=3.0.0");
   Assert (X.Value ("not (1 or 2) and 3").Synthetic_Image =
             "!(=1.0.0|=2.0.0)&=3.0.0");
   Assert (X.Value ("not (1 or (2 and 3)) and (4 or 5)").Synthetic_Image =
             "!(=1.0.0|(=2.0.0&=3.0.0))&(=4.0.0|=5.0.0)");
   Assert (X.Value ("not 1 and 2 and (3 or not 4)").Synthetic_Image =
             "!(=1.0.0)&=2.0.0&(=3.0.0|!(=4.0.0))");

   --  Mixing and/or with/without parentheses
   Assert (not X.Parse ("1 and 2 or 3").Valid);
   Assert (not X.Parse ("1 or 2 and 3").Valid);
   Assert (not X.Parse ("1 & 2 or 3").Valid);
   Assert (not X.Parse ("1 | 2 and 3").Valid);
   Assert (not X.Parse ("1 and 2 | 3").Valid);
   Assert (not X.Parse ("1 or 2 & 3").Valid);
   Assert (X.Value ("(1 and 2) | (3 and 4)").Synthetic_Image =
             "(=1.0.0&=2.0.0)|(=3.0.0&=4.0.0)");
   Assert (X.Value ("(1 or 2) & (3 or 4)").Synthetic_Image =
             "(=1.0.0|=2.0.0)&(=3.0.0|=4.0.0)");
   Assert (X.Value ("(1 & 2) or (3 & 4)").Synthetic_Image =
             "(=1.0.0&=2.0.0)|(=3.0.0&=4.0.0)");
   Assert (X.Value ("(1 | 2) and (3 | 4)").Synthetic_Image =
             "(=1.0.0|=2.0.0)&(=3.0.0|=4.0.0)");

   --  Some more
   Assert (X.Parse ("1 and not 2").Valid);
   Assert (X.Value ("1 and not 2").Synthetic_Image = "=1.0.0&!(=2.0.0)");
end Semver_Tests.Precedences;
