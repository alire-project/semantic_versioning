procedure Semver_Tests.Options is
   V1_2 : constant Version := V ("1.2.0");

   --  Test Unicode option for parsing
   Unicode_Opts : constant Basic.Options :=
     (Unicode        => True,
      Word_Operators => False,
      Implicit_Equal => False);

   No_Unicode_Opts : constant Basic.Options :=
     (Unicode        => False,
      Word_Operators => False,
      Implicit_Equal => False);

   --  Test Implicit_Equal option for output
   With_Implicit : constant Basic.Options :=
     (Unicode        => False,
      Word_Operators => False,
      Implicit_Equal => True);

   Without_Implicit : constant Basic.Options :=
     (Unicode        => False,
      Word_Operators => False,
      Implicit_Equal => False);

   VS1 : Basic.Version_Set;
   VS2 : Basic.Version_Set;

begin
   --  Test parsing with Unicode operators enabled
   VS1 := Basic.To_Set (U ("≥1.2.0"), Opts => Unicode_Opts);
   Assert (Basic.Is_In (V1_2, VS1), "Unicode >= parsing failed");
   Assert (Basic.Is_In (V ("1.3.0"), VS1), "Unicode >= parsing failed");

   VS1 := Basic.To_Set (U ("≤1.2.0"), Opts => Unicode_Opts);
   Assert (Basic.Is_In (V1_2, VS1), "Unicode <= parsing failed");
   Assert (Basic.Is_In (V ("1.1.0"), VS1), "Unicode <= parsing failed");

   VS1 := Basic.To_Set (U ("≠1.2.0"), Opts => Unicode_Opts);
   Assert (not Basic.Is_In (V1_2, VS1), "Unicode /= parsing failed");
   Assert (Basic.Is_In (V ("1.1.0"), VS1), "Unicode /= parsing failed");

   --  Test that Unicode operators are rejected when Unicode is disabled
   begin
      VS1 := Basic.To_Set (U ("≥1.2.0"), Opts => No_Unicode_Opts);
      Assert (False, "Should have raised exception for Unicode when disabled");
   exception
      when Malformed_Input =>
         null; -- Expected
   end;

   --  Test Image output with Implicit_Equal enabled
   VS1 := Basic.Exactly (V1_2);
   Assert (Basic.Image (VS1, Opts => With_Implicit) = "1.2.0",
           "Implicit_Equal output failed: " &
             Basic.Image (VS1, Opts => With_Implicit));

   --  Test Image output with Implicit_Equal disabled
   Assert (Basic.Image (VS1, Opts => Without_Implicit) = "=1.2.0",
           "Explicit = output failed: " &
             Basic.Image (VS1, Opts => Without_Implicit));

   --  Test Unicode output in Image
   VS1 := Basic.Except (V1_2);
   Assert (Basic.Image (VS1, Opts => Unicode_Opts) = U ("≠1.2.0"),
           "Unicode /= output failed: " &
             Basic.Image (VS1, Opts => Unicode_Opts));
   Assert (Basic.Image (VS1, Opts => No_Unicode_Opts) = "/=1.2.0",
           "ASCII /= output failed: " &
             Basic.Image (VS1, Opts => No_Unicode_Opts));

   VS1 := Basic.At_Least (V1_2);
   Assert (Basic.Image (VS1, Opts => Unicode_Opts) = U ("≥1.2.0"),
           "Unicode >= output failed: " &
             Basic.Image (VS1, Opts => Unicode_Opts));
   Assert (Basic.Image (VS1, Opts => No_Unicode_Opts) = ">=1.2.0",
           "ASCII >= output failed: " &
             Basic.Image (VS1, Opts => No_Unicode_Opts));

   VS1 := Basic.At_Most (V1_2);
   Assert (Basic.Image (VS1, Opts => Unicode_Opts) = U ("≤1.2.0"),
           "Unicode <= output failed: " &
             Basic.Image (VS1, Opts => Unicode_Opts));
   Assert (Basic.Image (VS1, Opts => No_Unicode_Opts) = "<=1.2.0",
           "ASCII <= output failed: " &
             Basic.Image (VS1, Opts => No_Unicode_Opts));

   --  Test Extended module with Options
   declare
      XVS1 : X.Version_Set;
      XVS2 : X.Version_Set;
   begin
      --  Parse with Unicode
      XVS1 := X.Value (U ("≥1.2.0"), Opts => Unicode_Opts);
      Assert (X.Is_In (V1_2, XVS1), "Extended Unicode parsing failed");

      --  Test Synthetic_Image with different options
      XVS1 := X.Value (">=1.2.0 & /=1.2.5");
      Assert (X.Synthetic_Image (XVS1, Opts => Unicode_Opts) =
                U ("≥1.2.0&≠1.2.5"),
              "Extended Unicode output failed: " &
                X.Synthetic_Image (XVS1, Opts => Unicode_Opts));

      Assert (X.Synthetic_Image (XVS1, Opts => No_Unicode_Opts) =
                ">=1.2.0&/=1.2.5",
              "Extended ASCII output failed: " &
                X.Synthetic_Image (XVS1, Opts => No_Unicode_Opts));

      --  Test Implicit_Equal with Extended
      XVS1 := X.Value ("=1.2.0");
      Assert (X.Synthetic_Image (XVS1, Opts => With_Implicit) = "1.2.0",
             "Extended Implicit_Equal output failed: " &
               X.Synthetic_Image (XVS1, Opts => With_Implicit));
      Assert (X.Synthetic_Image (XVS1, Opts => Without_Implicit) = "=1.2.0",
             "Extended explicit = output failed: " &
               X.Synthetic_Image (XVS1, Opts => Without_Implicit));
   end;

   --  Test Parse function with Options
   declare
      R : Basic.Result := Basic.Parse (U ("≥1.2.0 & ≤2.0.0"), Opts => Unicode_Opts);
   begin
      Assert (R.Valid, "Parse with Unicode failed");
      Assert (Basic.Is_In (V ("1.5.0"), R.Set),
              "Parsed set doesn't contain expected version");
   end;

   declare
      R : Basic.Result := Basic.Parse (U ("≥1.2.0"), Opts => No_Unicode_Opts);
   begin
      Assert (not R.Valid, "Parse should fail with Unicode when disabled");
   end;

   --  Test Value function with Options
   VS1 := Basic.Value (U ("≥1.2.0"), Opts => Unicode_Opts);
   Assert (Basic.Is_In (V ("1.5.0"), VS1),
           "Value with Unicode failed");

   begin
      VS1 := Basic.Value (U ("≥1.2.0"), Opts => No_Unicode_Opts);
      Assert (False, "Value should raise exception for Unicode when disabled");
   exception
      when Malformed_Input =>
         null; -- Expected
   end;

end Semver_Tests.Options;
