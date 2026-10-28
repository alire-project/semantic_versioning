procedure Semver_Tests.Relaxed_Parsing is
   --  Anything not conforming goes into the build metadata, so compare images
   --  (equality ignores build). By default it is made compliant; otherwise
   --  it is kept as is, which strict parsing would reject.
begin
   Assert
     (Image (Parse ("1.dfsg-3.1ubuntu2", Relaxed => True))
      = "1.0.0+dfsg-3.1ubuntu2");
   Assert
     (Image (Parse ("1.3.dfsg-3.1ubuntu2", Relaxed => True))
      = "1.3.0+dfsg-3.1ubuntu2");
   Assert
     (Image (Parse ("1.dfsg-3.1ubuntu2", Relaxed => True, Compliant => False))
      = "1.0.0+.dfsg-3.1ubuntu2");
   Assert
     (Image (Parse ("1.3.dfsg-3.1ubuntu2",
                    Relaxed => True, Compliant => False))
      = "1.3.0+.dfsg-3.1ubuntu2");
end Semver_Tests.Relaxed_Parsing;
