with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

--  Parse the first argument and re-emit its image.

procedure Semantic_Versioning.Extended.Parser is
   use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
begin
   if Argument_Count /= 1 then
      Put_Line ("Need exactly one argument (which is an extended version set)!");
      return;
   end if;

   declare
      Result : constant Extended.Result :=
                 Extended.Parse (Argument (1));
   begin
      if Result.Valid then
         Put_Line ("OK");
         Put_Line (Decode (Image (Result.Set)));
         Put_Line (Decode (Synthetic_Image (Result.Set)));
      else
         Put_Line (Decode ("Parse error: " & Result.Error));
      end if;
   exception
      when Malformed_Input =>
         Put_Line ("Uh oh... that was not a nice version!");
   end;
end Semantic_Versioning.Extended.Parser;
