with Ada.Exceptions;
with Ada.Strings.Fixed;

procedure Semver_Tests.Identifiers is
   --  Strict parsing checks pre-release and build metadata identifiers as
   --  semantic versioning requires: non-empty, made of [0-9A-Za-z-], and
   --  without leading zeros in numeric pre-release identifiers. Relaxed
   --  parsing does not check them, but makes them compliant (see the
   --  Compliant test).

   procedure Rejected (Text : String; Reason : String) is
   begin
      declare
         Unused : constant Version := Parse (Text);
      begin
         raise Assertion_Error with "Should have been rejected: " & Text;
      end;
   exception
      when E : Malformed_Input =>
         Assert (Ada.Strings.Fixed.Index
                   (Ada.Exceptions.Exception_Message (E), Reason) > 0,
                 "Unexpected reason for " & Text & ": "
                 & Ada.Exceptions.Exception_Message (E));
   end Rejected;

begin
   --  Examples from the specification

   Assert (Image (Parse ("1.0.0-alpha.1")) = "1.0.0-alpha.1");
   Assert (Image (Parse ("1.0.0-0.3.7")) = "1.0.0-0.3.7");
   Assert (Image (Parse ("1.0.0-x.7.z.92")) = "1.0.0-x.7.z.92");
   Assert (Image (Parse ("1.0.0-x-y-z.--")) = "1.0.0-x-y-z.--");
   Assert (Image (Parse ("1.0.0-beta+exp.sha.5114f85"))
           = "1.0.0-beta+exp.sha.5114f85");
   Assert (Image (Parse ("1.0.0+21AF26D3----117B344092BD"))
           = "1.0.0+21AF26D3----117B344092BD");

   --  Pre-release

   Rejected ("1.0.0-",     "Empty pre-release");
   Rejected ("1.0.0-+b",   "Invalid character in pre-release");
   Rejected ("1.0.0-a..b", "Empty identifier in pre-release");
   Rejected ("1.0.0-a.",   "Empty identifier in pre-release");
   Rejected ("1.0.0-.a",   "Empty identifier in pre-release");
   Rejected ("1.0.0-x_y",  "Invalid character in pre-release");
   Rejected ("1.0.0-01",   "leading zeros");

   --  Build metadata

   Rejected ("1.0.0+",     "Empty build metadata");
   Rejected ("1.0.0+a..b", "Empty identifier in build metadata");
   Rejected ("1.0.0+a+b",  "Invalid character in build metadata");
   Rejected ("1.0.0+a_b",  "Invalid character in build metadata");

   --  Relaxed parsing keeps anything that does not conform as build metadata

   Assert (Build (Parse ("1.0.0+a_b", Relaxed => True, Compliant => False))
           = "a_b");
end Semver_Tests.Identifiers;
