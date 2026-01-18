procedure Semver_Tests.With_Unicode is
   Unicode_Opts : constant Basic.Options :=
     (Unicode        => True,
      Word_Operators => False,
      Implicit_Equal => False);


begin
   Assert (X.Is_In (V ("1.1"), X.Value_U ("≠1")));
   Assert (X.Is_In (V ("1.1"), X.Value_U ("≥1")));
   Assert (X.Is_In (V ("1.1"), X.Value_U ("≤1.1")));

   declare
      XVS : constant X.Version_Set := X.Value_U ("¬1", Opts => Unicode_Opts);
   begin
      Assert (not X.Is_In (V ("1.0.0"), XVS));
      Assert (X.Synthetic_Image (XVS, Opts => Unicode_Opts) = U ("¬(=1.0.0)"));
   end;
end Semver_Tests.With_Unicode;
