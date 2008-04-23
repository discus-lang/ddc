
module Type.Util.Flatten 
	(flattenT)

where

import Type.Util.Bits
import Type.Exp

import Shared.Error
import Shared.Pretty
import Debug.Trace

import Util

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

	TForall vks t	-> TForall vks (down t)

	TFetters t fs
	 -> let (fsWhere, fsRest)
	 		= partition (=@= FLet{}) fs

		sub'	= Map.union 
				(Map.fromList $ map (\(FLet t1 t2) -> (t1, t2)) fsWhere)
				sub

		tFlat	= flattenT' sub' block t

	   in	addFetters fsRest tFlat

	TSum k ts	-> makeTSum  k (map down ts)
	TMask k t1 t2	-> makeTMask k (down t1) (down t2)

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

	TData k v ts		-> TData k v (map down ts)
	TFun t1 t2 eff clo	-> TFun (down t1) (down t2) (down eff) (down clo)

	TEffect v ts		-> TEffect v (map down ts)

	TFree v t		-> TFree v (down t)
	TDanger t1 t2		-> TDanger (down t1) (down t2)
	TTag v			-> TTag v

	TError{}		-> tt

	_			-> panic stage 
				$ "flattenT: no match for " % tt

