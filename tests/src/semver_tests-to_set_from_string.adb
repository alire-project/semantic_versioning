procedure Semver_Tests.To_Set_From_String is
begin
   Assert (Basic.Any = To_Set ("any"));
   Assert (Basic.Any = To_Set ("*"));

   Assert (Exactly (V1_0_0) = To_Set ("1.0.0"));
   Assert (Exactly (V1_0_0) = To_Set ("=1.0.0"));

   Assert (Except (V1_0_0) = To_Set ("/=1.0.0"));
   Assert (Except (V1_0_0) = To_Set_U ("≠1.0.0"));

   Assert (At_Least (V1_0_0) = To_Set (">=1.0.0"));
   Assert (At_Least (V1_0_0) = To_Set_U ("≥1.0.0"));

   Assert (At_Most (V1_0_0) = To_Set ("<=1.0.0"));
   Assert (At_Most (V1_0_0) = To_Set_U ("≤1.0.0"));

   Assert (More_Than (V1_0_0) = To_Set (">1.0.0"));
   Assert (Less_Than (V1_0_0) = To_Set ("<1.0.0"));
end Semver_Tests.To_Set_From_String;
