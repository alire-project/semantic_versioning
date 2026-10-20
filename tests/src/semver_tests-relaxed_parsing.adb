procedure Semver_Tests.Relaxed_Parsing is
   --  Anything not conforming goes into the build metadata, which strict
   --  parsing would reject, so compare images (equality ignores build).
begin
   Assert
     (Image (Parse ("1.dfsg-3.1ubuntu2", Relaxed => True))
      = "1.0.0+.dfsg-3.1ubuntu2");
   Assert
     (Image (Parse ("1.3.dfsg-3.1ubuntu2", Relaxed => True))
      = "1.3.0+.dfsg-3.1ubuntu2");
end Semver_Tests.Relaxed_Parsing;
