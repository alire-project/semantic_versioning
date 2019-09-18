with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

--  Parse the first argument and re-emit its image.

procedure Semantic_Versioning.Parser is
begin
   if Argument_Count /= 1 then
      Put_Line ("Need exactly one argument!");
      return;
   end if;

   declare
      V : Version;
   begin
      V := Parse (Argument (1), Relaxed => True);

      Put_Line (Image (V));
   exception
      when Malformed_Input =>
         Put_Line ("Uh oh... that was not a nice version!");
   end;
end Semantic_Versioning.Parser;
