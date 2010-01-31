
module Type.Util.Flatten 
	(flattenT)

where
import Type.Util.Bits
import Type.Exp
import Type.Pretty

import Shared.Error
import Shared.Pretty

import Util
import Debug.Trace
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Type.Util.Flatten"

-- | Flattening a type inlines all the (t1 = t2) fetters bound within in it.
flattenT :: Type -> Type
flattenT tt
 = flattenT' Map.empty Set.empty tt

flattenT' sub block tt
 = let down	= flattenT' sub block
   in  case tt of
   	TNil		-> TNil

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

	TSum k ts	-> makeTSum  k (map down ts)

	TApp t1 t2	-> TApp (down t1) (down t2)
	TCon{}		-> tt

	TVar{}
	 | Set.member tt block
	 -> tt

	 | otherwise
	 -> case Map.lookup tt sub of
	 	Just t	-> flattenT' sub (Set.insert tt block) t
		Nothing	-> tt

	TClass{}
	 | Set.member tt block
	 -> tt

	 | otherwise
	 -> case Map.lookup tt sub of
	 	Just t	-> flattenT' sub (Set.insert tt block) t
		Nothing	-> tt

	TTop{}			-> tt
	TBot{}			-> tt

	TEffect v ts		-> TEffect v (map down ts)

	TFree v t		-> TFree v (down t)
	TDanger t1 t2		-> TDanger (down t1) (down t2)

	TError{}		-> tt

	_			-> panic stage 
				$ "flattenT: no match for " % tt

