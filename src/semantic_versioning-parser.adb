with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

--  Parse the first argument and re-emit its image.

procedure Semantic_Versioning.Parser is
   use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
begin
   if Argument_Count /= 1 then
      Put_Line ("Need exactly one argument!");
      return;
   end if;

   declare
      V : Version;
   begin
      V := Parse (Argument (1), Relaxed => True);

      Put_Line (Decode (Image (V)));
   exception
      when E : Malformed_Input =>
         Put_Line ("Uh oh... that was not a nice version: "
                   & Decode (Ada.Exceptions.Exception_Message (E)));
   end;
end Semantic_Versioning.Parser;
