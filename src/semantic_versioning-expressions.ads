generic
   type LH (<>) is private;
   type Result (<>) is private;
   with function Build_Condition (L : LH; VS : Version_Set) return Result;
   Strict : Boolean := False; -- When parsing strings
package Semantic_Versioning.Expressions with Preelaborate is

   --  Helper package to build more naturally-looking expressions

   --  Direct comparison against versions: LH >= "1.0", LH >= V_1.0

   function "<" (L : LH; R : Version) return Result;
   function "<" (L : LH; R : String)  return Result;

   function "<=" (L : LH; R : Version) return Result;
   function "<=" (L : LH; R : String)  return Result;

   function "=" (L : LH; R : Version) return Result;
   function "=" (L : LH; R : String)  return Result;

   function ">=" (L : LH; R : Version) return Result;
   function ">=" (L : LH; R : String)  return Result;

   function ">" (L : LH; R : Version) return Result;
   function ">" (L : LH; R : String)  return Result;

   function "/=" (L : LH; R : Version) return Result;
   function "/=" (L : LH; R : String)  return Result;

private

   function "<" (L : LH; R : Version) return Result is (Build_Condition (L, Less_Than (R)));
   function "<" (L : LH; R : String)  return Result is (L < Parse (R, not Strict));

   function "<=" (L : LH; R : Version) return Result is (Build_Condition (L, At_Most (R)));
   function "<=" (L : LH; R : String)  return Result is (L <= Parse (R, not Strict));

   function "=" (L : LH; R : Version) return Result is (Build_Condition (L, Exactly (R)));
   function "=" (L : LH; R : String)  return Result is (L = Parse (R, not Strict));

   function ">=" (L : LH; R : Version) return Result is (Build_Condition (L, At_Least (R)));
   function ">=" (L : LH; R : String)  return Result is (L >= Parse (R, not Strict));

   function ">" (L : LH; R : Version) return Result is (Build_Condition (L, More_Than (R)));
   function ">" (L : LH; R : String)  return Result is (L > Parse (R, not Strict));

   function "/=" (L : LH; R : Version) return Result is (Build_Condition (L, Less_Than (R)));
   function "/=" (L : LH; R : String)  return Result is (L /= Parse (R, not Strict));

end Semantic_Versioning.Expressions;
