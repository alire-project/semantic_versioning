with Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.IO; use GNAT.IO;

package body Semantic_Versioning.Extended is

   package ACH renames Ada.Characters.Handling;

   use type Ada.Containers.Count_Type;

   Debug : constant Boolean := False;

   -----------
   -- Trace --
   -----------

   procedure Trace (Str : String) is
   begin
      if Debug then
         Put_Line (Str);
      end if;
   end Trace;

   type List_Kinds is (Any, Anded, Ored);
   --  Used to unify a few productions by parameterizing one.

   subtype Concrete_List_Kinds is List_Kinds range Anded .. Ored;
   Concrete_Images : constant array (Concrete_List_Kinds) of Character :=
                       (Anded => '&',
                        Ored  => '|');

   --  Simpler tree manipulation

   -----------------
   -- Root_Cursor --
   -----------------

   function Root_Cursor (Leaf : Version_Set) return Trees.Cursor is
   begin
      return Trees.First_Child (Leaf.Set.Root);
   end Root_Cursor;

   --------------
   -- New_Leaf --
   --------------

   function New_Leaf (BVS : Basic.Version_Set;
                      Img : String) return Version_Set is
      VS : Version_Set;
   begin
      VS.Image := To_Unbounded_String (Img);
      VS.Set.Append_Child (VS.Set.Root,
                           Any_Node'(Kind => Leaf,
                                     VS   => BVS));
      Trace ("Creating leaf: " & Img);
      return VS;
   end New_Leaf;

   --------------
   -- New_Tree --
   --------------

   function New_Tree (Pos : Trees.Cursor) return Version_Set is
      --  Create a temporary version set with this subtree, so it can be
      --  compared via synthetic image,
      Tree : Trees.Tree;
   begin
      Trees.Copy_Subtree (Target => Tree,
                          Parent => Tree.Root,
                          Before => Trees.First_Child (Tree.Root),
                          Source => Pos);
      return Version_Set'(Set   => Tree,
                          Image => UStrings.Null_Unbounded_String);
   end New_Tree;

   --------------
   -- New_Pair --
   --------------

   function New_Pair (L, R : Version_Set;
                      Kind : Concrete_List_Kinds) return Version_Set
   is
      Link : constant Any_Node :=
               (case Kind is
                   when Anded => (Kind => Anded),
                   when Ored  => (Kind => Ored)); -- Must be static expr.
      Link_Tree : Version_Set;
      Link_Pos  : Trees.Cursor;

      --------------
      -- Contains --
      --------------

      function Contains (Pos : Trees.Cursor; VS : Version_Set) return Boolean
      is
         use Trees;
         Pos_Kind : constant Kinds := Element (Pos).Kind;
      begin

         --  Check subtrees for a match with the same operation

         for Child in Iterate_Subtree (Pos) loop
            if not Is_Root (Parent (Child)) and then   -- Root lacks operation
              Pos_Kind = Element (Parent (Child)).Kind -- Operation matches
            then
               if New_Tree (Child) = VS then
                  return True;
               end if;
            end if;
         end loop;

         return False;
      end Contains;

   begin

      --  See if this can be simplified, starting with trivial simplifications

      if L = R then
         return L;
      elsif Kind = Ored and then (L = Any or R = Any) then
         return Any;
      elsif Kind = Anded and then L = Any then
         return R;
      elsif Kind = Anded and then R = Any then
         return L;
      end if;

      --  Recursive simplifications

      if Contains (L.Root_Cursor, R) then
         return L;
      elsif Contains (R.Root_Cursor, L) then
         return R;
      end if;

      --  Proceed with construction

      Link_Tree.Set.Append_Child (Link_Tree.Set.Root, Link);
      Link_Pos := Trees.First_Child (Link_Tree.Set.Root);

      Link_Tree.Set.Copy_Subtree (Parent => Link_Pos,
                                  Before => Trees.No_Element,
                                  Source => Root_Cursor (L));
      Link_Tree.Set.Copy_Subtree (Parent => Link_Pos,
                                  Before => Trees.No_Element,
                                  Source => Root_Cursor (R));
      Link_tree.Image :=
        "(" & L.Image & ") "
        & Concrete_Images (Kind)
        & " (" & R.Image & ")";

      return Link_Tree;
   end New_Pair;

   ---------
   -- Any --
   ---------

   function Any return Version_Set is
     (To_Extended (Basic.Any));

   ------------
   -- Is_Any --
   ------------

   function Is_Any (VS : Version_Set) return Boolean is
     (Vs = Any);

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Version_Set) return Boolean is
     (L.Synthetic_Image = R.Synthetic_Image);

   -----------
   -- "not" --
   -----------

   function "not" (VS : Version_Set) return Version_Set is
      Not_Node : constant Any_Node := (Kind => Negated);
   begin
      return Result : Version_Set :=
        Version_Set'(Set   => Trees.Empty_Tree,
                     Image => "!(" & VS.Image & ")")
      do
         Trace ("Creating not node: " & Image (Result));
         Result.Set.Append_Child (Result.Set.Root,
                                  Not_Node);
         Result.Set.Copy_Subtree
           (Parent => Trees.First_Child (Result.Set.Root),
            Before => Trees.No_Element,
            Source => Trees.First_Child (VS.Set.Root));
      end return;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Version_Set) return Version_Set is
      (New_Pair (L, R, Anded));

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Version_Set) return Version_Set is
      (New_Pair (L, R, Ored));

   -----------
   -- Is_In --
   -----------

   function Is_In (V : Version; VS : Version_Set) return Boolean is

      -----------
      -- Is_In --
      -----------

      function Is_In (Pos : Trees.Cursor) return Boolean is
         Node : Any_Node renames Trees.Element (Pos);
      begin
         case Node.Kind is
            when Leaf =>
               Trace ("Leaf: " & Basic.Image_Abbreviated (Node.Vs)
                      & " " & Basic.Is_In (V, Node.VS)'Img);
               return Basic.Is_In (V, Node.VS);

            when Anded =>
               return OK : Boolean := True do
                  Trace ("AND children count:" & Trees.Child_Count (Pos)'Img);
                  for Child in VS.Set.Iterate_Children (Pos) loop
                     OK := OK and then Is_In (Child);
                  end loop;
               end return;

            when Ored =>
               return OK : Boolean := False do
                  Trace ("OR children count:" & Trees.Child_Count (Pos)'Img);
                  for Child in VS.Set.Iterate_Children (Pos) loop
                     OK := OK or else Is_In (Child);
                  end loop;
               end return;

            when Negated =>
               Trace ("Evaluating NOT node with child count: "
                      & Trees.Child_Count (Pos)'Img);
               pragma Assert (Trees.Child_Count (Pos) = 1);
               return not Is_In (Trees.First_Child (Pos));

         end case;
      end Is_In;

   begin
      if VS.Set.Is_Empty then
         Trace ("Is_In: EMPTY");
         return False;
      else
         return Is_In (Trees.First_Child (VS.Set.Root));
      end if;
   end Is_In;

   -----------------------
   -- Is_Single_Version --
   -----------------------

   function Is_Single_Version (VS : Version_Set) return Boolean is
     (Trees.Child_Count (VS.Set.Root) = 1
      and then Trees.Element (Root_Cursor (VS)).Kind = Leaf
      and then Basic.Is_Single_Version (Trees.Element (Root_Cursor (VS)).VS));

   -----------
   -- Image --
   -----------

   function Image (VS : Version_Set) return String is
     (To_String (VS.Image));

   --------------
   -- Simplify --
   --------------

   function Simplify (VS : Version_Set) return Version_Set
   is (VS);
   --  This is a fake function so it can be explained in the spec.
   --  Simplifications occur at New_Pair, to avoid the problem of the
   --  lost original image of subtrees

   ---------------------
   -- Synthetic_Image --
   ---------------------

   function Synthetic_Image (VS      : Version_Set;
                             Unicode : Boolean := False) return String is

      function Img (Pos : Trees.Cursor) return String is
         Node : Any_Node renames Trees.Element (Pos);

         function List_Img return String is
            List : Ustring;
            I    : Trees.Cursor := Trees.First_Child (Pos);
            First : Boolean := True;
         begin
            while Trees.Has_Element (I) loop
               if First then
                  First := False;
               else
                  List := List & Concrete_Images
                    (List_Kinds'Value (Node.Kind'Img));
               end if;
               if Trees.Element (I).Kind not in Leaf | Node.Kind | Negated then
                  List := List & "(";
               end if;
               List := List & Img (I);
               if Trees.Element (I).Kind not in Leaf | Node.Kind | Negated then
                  List := List & ")";
               end if;
               I := Trees.Next_Sibling (I);
            end loop;
            return To_String (List);
         end List_Img;

      begin
         case Node.Kind is
            when Leaf =>
               return Basic.Image_Abbreviated (Node.VS,
                                               Unicode => Unicode);

            when Anded | Ored =>
               return List_Img;

            when Negated =>
               return "!(" & Img (Trees.First_Child (Pos)) & ")";
         end case;
      end Img;

   begin
      if VS.Set.Is_Empty then
         return "(empty extended version set)";
      else
         return Img (Trees.First_Child (VS.Set.Root));
      end if;
   end Synthetic_Image;

   -----------------
   -- To_Extended --
   -----------------

   function To_Extended (BVS : Basic.Version_Set)
                         return Version_Set is
     (New_Leaf (BVS, Basic.Image_Abbreviated (BVS)));

   -----------
   -- Parse --
   -----------

   function Parse (Str     : String;
                   Relaxed : Boolean := False;
                   Unicode : Boolean := True) return Result is

      --  See grammar.txt for the recursive parser being implemented here.
      --  Since there is no need to backtrack, we can store here the partial
      --  solution:

      Parse_Error : exception;

      type Tokens is (Ampersand,
                      Lparen,
                      Rparen,
                      Number,
                      Pipe,
                      Negation,
                      Unknown,
                      VS,
                      End_Of_Input);

      I   : Integer := Str'First; -- Next char to process
      Err : Unbounded_String;

      -----------
      -- Error --
      -----------
      --  Set error and raise
      procedure Error (Msg : String; With_Pos : Boolean := True) is
         Extended_Msg : constant String :=
                          (if With_Pos
                           then Msg & " (at char" & Integer'(I - Str'First + 1)'Img & ")"
                           else Msg);
      begin
         Err := To_Unbounded_String (Extended_Msg);
         raise Parse_Error;
      end Error;

      -----------
      -- Match --
      -----------

      procedure Match (C : Character) is
      begin
         if I > Str'Last then
            Error ("Incomplete expression when expecting character: " & C);
         elsif Str (I) /= C then
            Error ("Got a '" & Str (I) & "' when expecting a '" & C & "'");
         else
            Trace ("Matching " & C & " at pos" & I'Img);
            I := I + 1;
         end if;
      end Match;

      -------------------
      -- Next_Basic_VS --
      -------------------
      --  Get all characters until something that is not a version set
      function Next_Basic_VS return String is
         Last : Natural := I;
      begin
         if I > Str'Last then
            Error ("Incomplete expression when expecting a version set");
         end if;

         while Last <= Str'Last and then
           Str (Last) not in '&' | '|' | ')' | ' ' | '('
         loop
            Last := Last + 1;
         end loop;

         if Last = I then
            Error ("Empty version set substring");
         end if;

         return Substr : constant String := Str (I .. Last - 1) do
            I := Last;
         end return;
      end Next_Basic_VS;

      ----------------
      -- Next_Token --
      ----------------

      function Next_Token (Skip_Whitespace : Boolean := True) return Tokens is
         function Internal return Tokens is
         begin
            if I > Str'Last then
               return End_Of_Input;
            end if;

            if Skip_Whitespace then
               while I <= Str'Last and then Str (I) = ' ' loop
                  I := I + 1;
               end loop;
            end if;

            if I > Str'Last then
               return End_Of_Input;
            end if;

            if Begins_With_Relational (Str (I .. Str'Last), Unicode) then
               return VS;
            end if;

            case Str (I) is
               when '&'                   => return Ampersand;
               when '('                   => return Lparen;
               when ')'                   => return Rparen;
               when '0' .. '9'            => return Number;
               when '|'                   => return Pipe;
               when '!'                   => return Negation;
               when '<' | '>' | '='
                        | '/' | '~' | '^' => return VS; -- already checked above, but...
               when others                => return Unknown;
            end case;
         end Internal;
      begin
         return T : constant Tokens := Internal do
            Trace ("Next token: " & T'Img);
         end return;
      end Next_Token;

      function Prod_EVS_Nested return Version_Set;
      function Prod_List (Head : Version_Set;
                          Kind : List_Kinds) return Version_Set;
      function Prod_VS return Version_Set;

      --------------
      -- Prod_EVS --
      --------------

      function Prod_EVS (List_Kind : List_Kinds; With_List : Boolean)
                         return Version_Set
      is
         Next : Version_Set;
      begin
         Trace ("Prod EVS");
         case Next_Token is
            when Negation =>
               Match ('!');
               declare
                  Child : constant Version_Set := Prod_EVS (List_Kind, With_List => False);
               begin
                  if Trees.First_Child_Element (Child.Set.Root).Kind = Negated then
                     Error ("Double negation");
                  end if;
                  Next := not Child;
               end;


            when Lparen =>
               Next := Prod_EVS_Nested;

            when Number | VS =>
               Next := Prod_VS;

            when End_Of_Input =>
               Error ("Incomplete expression");
               return Empty_Set; -- Unreachable, Error raises.

            when others =>
               Error ("Unexpected symbol: " & Str (I));
               return Empty_Set; -- Unreachable, Error raises.
         end case;

         --  Optional continuation list
         if With_List then  --  I suspect this is flying on the face of LR(1)
            if Next_Token in Ampersand | Pipe then
               Trace ("Prod EVS: optional list present");
               return Prod_List (Next, List_Kind);
            else
               Trace ("Prod EVS: optional list missing");
               return Next;
            end if;
         else
            return Next;
         end if;
      end Prod_EVS;

      ---------------------
      -- Prod_EVS_Nested --
      ---------------------

      function Prod_EVS_Nested return Version_Set is
      begin
         Trace ("Prod EVS Nested");
         Match ('(');
         return VS : Version_Set := Prod_EVS (Any, With_List => True) do
            VS.Image := '(' & VS.Image & ')';
            Match (')');
         end return;
      end Prod_EVS_Nested;

      ---------------
      -- Prod_List --
      ---------------

      function Prod_List (Head : Version_Set;
                          Kind : List_Kinds) return Version_Set
      is

         --------------------
         -- Check_Mismatch --
         --------------------

         procedure Check_Mismatch is
         begin
            if I <= Str'Last then
               if (Kind = Anded and then Str (I) = '|') or else
                 (Kind = Ored and then Str (I) = '&')
               then
                  Error ("Cannot mix '&' and '|' operators, use parentheses");
               end if;
            end if;
         end Check_Mismatch;

      begin
         Trace ("Prod List " & Kind'Img &  " with head " & Image (Head));
         case Kind is
            when Any =>
               case Next_Token is
                  when Ampersand =>
                     return Prod_List (Head, Anded);
                  when Pipe      =>
                     return Prod_List (Head, Ored);
                  when others    =>
                     Error ("Unexpected list concatenator: " & Str (I));
                     return Empty_Set; -- Unreachable, Error raises.
               end case;

            when Anded =>
               Check_Mismatch;
               Match ('&');
               return New_Pair (Head, Prod_EVS (Anded, With_List => True), Anded);

            when Ored =>
               Check_Mismatch;
               Match ('|');
               return New_Pair (Head, Prod_EVS (Ored, With_List => True), Ored);
         end case;
      end Prod_List;

      -------------
      -- Prod_VS --
      -------------

      function Prod_VS return Version_Set is
         BVS_Image : constant String := Next_Basic_VS;
      begin
         Trace ("Prod VS");
         return New_Leaf (Basic.To_Set (S       => BVS_Image,
                                        Relaxed => Relaxed,
                                        Unicode => Unicode), BVS_Image);
      exception
         when Malformed_Input =>
            Error ("Malformed basic version set: " & BVS_Image);
            return Empty_Set; -- Unreachable
      end Prod_VS;

   begin
      --  Special cases first
      if ACH.To_Lower (Str) = "any" or else Str = "*" then
         return New_Valid_Result (To_Extended (Basic.Any));
      end if;

      return Set : Result := New_Valid_Result (Prod_EVS (Any, With_List => True)) do
         if Next_Token /= End_Of_Input then
            Error ("Unexpected input after parsing version set: " & Str (I));
         else
            Set.Set.Image := To_Unbounded_String (Str);
         end if;
      end return;
   exception
      when E : Parse_Error =>
         if Debug then
            Put_Line (Exception_Information (E));
         end if;

         return (Valid  => False,
                 Length => Length (Err),
                 Error  => To_String (Err));
   end Parse;

   -----------
   -- Value --
   -----------

   function Value (Str     : String;
                   Relaxed : Boolean := False;
                   Unicode : Boolean := True) return Version_Set
   is
      R : constant Result := Parse (Str     => Str,
                                    Relaxed => Relaxed,
                                    Unicode => Unicode);
   begin
      if R.Valid then
         return R.Set;
      else
         raise Malformed_Input with R.Error;
      end if;
   end Value;

end Semantic_Versioning.Extended;
