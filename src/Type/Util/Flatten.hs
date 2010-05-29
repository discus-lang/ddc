
module Type.Util.Flatten 
	(flattenT)
where
import Type.Util.Bits
import Util
import DDC.Type.Exp
import DDC.Type.Predicates
import DDC.Type.Compounds
import Type.Pretty		()
import qualified Data.Map	as Map
import qualified Data.Set	as Set

-- | Flattening a type inlines all the (t1 = t2) fetters bound within in it.
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

	TFetters t fs
	 -> let (fsWhere, fsRest)
	 		= partition isFWhere fs

		sub'	= Map.union 
				(Map.fromList $ map (\(FWhere t1 t2) -> (t1, t2)) fsWhere)
				sub

		tFlat	= flattenT' sub' block t

	   in	addFetters fsRest tFlat

	TConstrain t crs
	 -> flattenT' sub block
	 $  toFetterFormT tt

	TVar{}
	 | Set.member tt block
	 -> tt

	 | otherwise
	 -> case Map.lookup tt sub of
	 	Just t	-> flattenT' sub (Set.insert tt block) t
		Nothing	-> tt

	TError{}	-> tt
