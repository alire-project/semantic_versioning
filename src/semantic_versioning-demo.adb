procedure Semantic_Versioning.Demo is
   V1_0_0 : constant Version := New_Version (1);
   V1_1_0 : constant Version := New_Version (1, 1);
   V1_1_1 : constant Version := New_Version (1, 1, 1);
begin
   pragma Assert (New_Version (1, 2, 3) = New_Version ("1.2.3"));

   pragma Assert (Satisfies (V1_0_0, At_Least (V1_0_0)));
   pragma Assert (Satisfies (V1_0_0, At_Most  (V1_0_0)));
   pragma Assert (Satisfies (V1_0_0, Exactly  (V1_0_0)));

   pragma Assert (not Satisfies (V1_0_0, Less_Than (V1_0_0)));
   pragma Assert (not Satisfies (V1_0_0, Except    (V1_0_0)));

   pragma Assert (Satisfies (V1_0_0, Less_Than (V1_1_0)));
   pragma Assert (Satisfies (V1_0_0, Except    (V1_1_0)));

   pragma Assert (Satisfies (V1_1_0, At_Least (V1_0_0) and At_Most (V1_1_1)));
end Semantic_Versioning.Demo;
