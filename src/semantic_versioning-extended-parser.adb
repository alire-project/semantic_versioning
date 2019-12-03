with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

--  Parse the first argument and re-emit its image.

procedure Semantic_Versioning.Extended.Parser is
begin
   if Argument_Count /= 1 then
      Put_Line ("Need exactly one argument (which is an extended version set)!");
      return;
   end if;

   declare
      Result : constant Extended.Result := Extended.Parse (Argument (1));
   begin
      if Result.Valid then
         Put_Line ("OK");
         Put_Line (Image (Result.Set));
         Put_Line (Synthetic_Image (Result.Set));
      else
         Put_Line ("Parse error: " & Result.Error);
      end if;
   exception
      when Malformed_Input =>
         Put_Line ("Uh oh... that was not a nice version!");
   end;
end Semantic_Versioning.Extended.Parser;
