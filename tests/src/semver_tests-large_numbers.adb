procedure Semver_Tests.Large_Numbers is

   Max : constant String := "999999999999999999";
   --  The largest number accepted in any point, see Semantic_Versioning.Point

   Max_Plus_One : constant String := "1000000000000000000";

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

   procedure Rejected_In_All_Points (Number : String) is
   begin
      Rejected (Number & ".0.0");
      Rejected ("0." & Number & ".0");
      Rejected ("0.0." & Number);
   end Rejected_In_All_Points;

begin
   --  The largest number is accepted in every point

   Assert (Major (Parse (Max & ".0.0")) = Point'Last);
   Assert (Minor (Parse ("0." & Max & ".0")) = Point'Last);
   Assert (Patch (Parse ("0.0." & Max)) = Point'Last);
   Assert (Image (Parse (Max & "." & Max & "." & Max))
           = Max & "." & Max & "." & Max);

   --  Anything larger is malformed input, in every point, instead of raising
   --  Constraint_Error.

   Rejected_In_All_Points (Max_Plus_One);
   Rejected_In_All_Points (Max & "9");
end Semver_Tests.Large_Numbers;
