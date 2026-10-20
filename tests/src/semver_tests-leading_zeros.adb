procedure Semver_Tests.Leading_Zeros is
   --  Semantic versioning forbids leading zeros in X.Y.Z and in numeric
   --  pre-release identifiers. Strict parsing rejects them, while relaxed
   --  parsing still accepts them.

   procedure Rejected (Text : String) is
   begin
      declare
         Unused : constant Version := Parse (Text);
      begin
         raise Assertion_Error with "Should have been rejected: " & Text;
      end;
   exception
      when Malformed_Input =>
         null; -- Expected
   end Rejected;

begin
   --  Rejected in strict mode

   Rejected ("01.0.0");
   Rejected ("1.01.0");
   Rejected ("1.0.01");
   Rejected ("00.0.0");
   Rejected ("2025.04.18");
   Rejected ("1.0.0-01");
   Rejected ("1.0.0-rc.01");
   Rejected ("1.0.0-01.rc");

   --  Zero itself, and zeros that are not leading, are fine

   Assert (Image (Parse ("0.0.0")) = "0.0.0");
   Assert (Image (Parse ("10.20.300")) = "10.20.300");
   Assert (Image (Parse ("1.0.0-0")) = "1.0.0-0");
   Assert (Image (Parse ("1.0.0-rc.0")) = "1.0.0-rc.0");

   --  Alphanumeric pre-release identifiers and build metadata may begin with
   --  zeros.

   Assert (Image (Parse ("1.0.0-0a")) = "1.0.0-0a");
   Assert (Image (Parse ("1.0.0+001")) = "1.0.0+001");
   Assert (Image (Parse ("1.0.0-rc.1+001.002")) = "1.0.0-rc.1+001.002");

   --  Relaxed parsing still accepts leading zeros

   Assert (Parse ("01.0.0", Relaxed => True) = V ("1.0.0"));
   Assert (Parse ("2025.04.18", Relaxed => True) = V ("2025.4.18"));
end Semver_Tests.Leading_Zeros;
