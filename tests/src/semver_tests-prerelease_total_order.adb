procedure Semver_Tests.Prerelease_Total_Order is

   --  Ascending array of versions covering the cases from issue #26:
   --  numeric fields sorted numerically at arbitrary precision (no Integer
   --  overflow), then alphanumeric fields sorted lexicographically, with
   --  every numeric field below every alphanumeric one (Semver 2.0.0 §11.4).
   --  This also covers the numeric-vs-alphanumeric cycle from the issue
   --  (9 / 10 / 1a) and a case that used to sort wrongly the other way
   --  around (2 / 10a).
   A : constant array (Positive range <>) of Version :=
     (V ("1.0.0-2"),
      V ("1.0.0-9"),
      V ("1.0.0-10"),
      V ("1.0.0-2000"),
      V ("1.0.0-9999999999999999999"),
      V ("1.0.0-10000000000000000000"),
      V ("1.0.0-10a"),
      V ("1.0.0-1a"),
      V ("1.0.0-1e3"),
      V ("1.0.0"));
begin
   --  The issue's reproducer: with the old rule, "9" < "10" < "1a" < "9" cycled.
   Assert (V ("1.0.0-9")  < V ("1.0.0-10"));
   Assert (V ("1.0.0-10") < V ("1.0.0-1a"));
   Assert (not (V ("1.0.0-1a") < V ("1.0.0-9")));

   --  Numeric identifiers always have lower precedence than alphanumeric ones.
   Assert (V ("1.0.0-2") < V ("1.0.0-10a"));

   --  Arbitrary-precision numeric comparison, no Integer overflow.
   Assert (V ("1.0.0-9999999999999999999") < V ("1.0.0-10000000000000000000"));

   --  Ada numeral syntax is no longer special: "1e3" is alphanumeric, not
   --  the number 1000, so it ranks above every numeric field, even "1001".
   Assert (V ("1.0.0-1001") < V ("1.0.0-1e3"));

   --  Full strict-total-order sweep: for every pair in the ascending array
   --  above, "<" and "=" must agree with the array's order.
   for I in A'Range loop
      for J in A'Range loop
         if I < J then
            Assert (A (I) < A (J));
            Assert (not (A (J) < A (I)));
            Assert (A (I) /= A (J));
         elsif I = J then
            Assert (not (A (I) < A (J)));
            Assert (A (I) = A (J));
         end if;
      end loop;
   end loop;
end Semver_Tests.Prerelease_Total_Order;
