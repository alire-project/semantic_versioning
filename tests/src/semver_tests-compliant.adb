procedure Semver_Tests.Compliant is
   --  Relaxed parsing makes the pre-release and build metadata comply with
   --  the specification by default, so that the result can be written and
   --  parsed back strictly. Versions as reported by system package managers.

   procedure Check (Text, Expected : String) is
      Image_Of : constant String := Image (Relaxed (Text));
   begin
      Assert (Image_Of = Expected,
              "Relaxed " & Text & " gave " & Image_Of & ", expected "
              & Expected);
      --  Must not raise
      Assert (Image (Parse (Image_Of)) = Image_Of,
              "Not accepted by strict parsing: " & Image_Of);
   end Check;

begin
   --  Homebrew and MacPorts revisions
   Check ("3.8.13_2", "3.8.13+-2");

   --  Debian/Ubuntu
   Check ("0.5.1~git2020-1", "0.5.1+-git2020-1");
   Check ("1.2.3-1ubuntu0.1~22.04", "1.2.3-1ubuntu0.1-22.4");
   Check ("1.2+dfsg+1", "1.2.0+dfsg-1");
   Check ("2.36.1-8+deb11u1", "2.36.1-8+deb11u1");

   --  Empty identifiers and leading zeros
   Check ("1.0.0-01", "1.0.0-1");
   Check ("1.0.0-00.a", "1.0.0-0.a");
   Check ("1.0.0+a..b", "1.0.0+a.b");
   Check ("1.0.0+007", "1.0.0+007"); -- Allowed in build metadata
   Check ("1.0.0+.", "1.0.0");
   Check ("1.0.0-..", "1.0.0");

   --  Already compliant versions are unchanged
   Check ("1.0.0-alpha.1+exp.sha.5114f85", "1.0.0-alpha.1+exp.sha.5114f85");

   --  Compliant is the default, and can be disabled
   Assert (Image (Parse ("3.8.13_2", Relaxed => True)) = "3.8.13+-2");
   Assert (Image (Relaxed ("3.8.13_2", Compliant => False)) = "3.8.13+_2");

   --  It does not affect strict parsing
   Assert (Image (Parse ("1.0.0+a", Compliant => False)) = "1.0.0+a");
end Semver_Tests.Compliant;
