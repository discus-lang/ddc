{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Type.Operators.Flatten
	(flattenT)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "DDC.Type.Flatten"

-- | Flatten a type by inlining all the equality constraints in it.
--   We keep track of the constraints substituted on the way down the tree, 
--   and panic if any are found to be recursive.
flattenT :: Type -> Type
flattenT tt
 = flattenT' Map.empty Set.empty tt

flattenT' sub block tt
 = let down	= flattenT' sub block
   in  case tt of
   	TNil		-> TNil

	TSum k ts	-> makeTSum  k (map down ts)
	TApp t1 t2	-> TApp (down t1) (down t2)
	TCon{}		-> tt
	TForall b k t	-> TForall b k (down t)

	TConstrain t crs
	 -> let sub'	= Map.union sub (crsEq crs)
		tFlat	= flattenT' sub' block t
	    in	makeTConstrain tFlat (Constraints Map.empty (crsMore crs) (crsOther crs))
	
	TVar{}
	 | Set.member tt block
	 -> panic stage $ "flattenT: recursive substitution through " % show tt

	 | otherwise
	 -> case Map.lookup tt sub of
	 	Just t	-> flattenT' sub (Set.insert tt block) t
		Nothing	-> tt

	TError{}	-> tt
