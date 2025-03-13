procedure Semver_Tests.Relaxed_Parsing is
begin
   Assert
     (Parse ("1.dfsg-3.1ubuntu2", Relaxed => True)
      = V ("1+.dfsg-3.1ubuntu2"));
   Assert
     (Parse ("1.3.dfsg-3.1ubuntu2", Relaxed => True)
      = V ("1.3+.dfsg-3.1ubuntu2"));
end Semver_Tests.Relaxed_Parsing;
