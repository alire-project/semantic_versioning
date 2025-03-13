procedure Semver_Tests.Extended_Simplifications is
begin
   Assert (X.Value ("1") = (X.Value ("1") and X.Value ("1")));
   Assert (X.Value ("*") = (X.Value ("*") or X.Value ("1.0")));
   Assert (X.Value ("1.0") = (X.Value ("*") and X.Value ("1.0")));
   Assert ((X.Value ("1") and
             (X.Value ("2") and X.Value ("1"))) =
           (X.Value ("2") and X.Value ("1")));
   Assert ((X.Value ("1") or
             (X.Value ("2") or
                  (X.Value ("3") or X.Value ("1")))) =
           (X.Value ("2") or
             (X.Value ("3") or X.Value ("1"))));
end Semver_Tests.Extended_Simplifications;
