
module Type.ToCore
	( punctureInstMapT
	, punctureInst
	, toCoreT
	, toCoreK
	, toCoreF )
where

-----
import Util
import Shared.Error		(panic)
import Shared.Var		(Var)
import Util.Pretty
import Util.Maybe
import qualified Debug.Trace	as Debug
import qualified Shared.Var 	as Var
import qualified Shared.VarUtil	as Var

import qualified Type.Exp	as T
import qualified Type.Pretty	as T

import qualified Core.Exp 	as C
import qualified Core.Util	as C
import qualified Core.Pack	as C

-----
stage	= "Type.ToCore"

-----
punctureInstMapT 
	:: T.Type -> [Int]
	
punctureInstMapT t
 = let	(vks, fs, x)	= stripQuantFsT t
   in	[n	| (n, (v, k)) <- zip [0..] vks
   		, not $ hasFetter v fs]
		


punctureInst
	:: [Int] -> [a] -> [a]
	
punctureInst pMap inst
	= [i 	| (n, i) <- zip [0..] inst
		, elem n pMap]
		

-----

stripQuantFsT 
	:: T.Type 
	-> ([(Var, T.Kind)], [T.Fetter], T.Type)

stripQuantFsT	t
 = case t of
 	T.TForall vks x	-> stripQuantFsT2 vks 	x
	_		-> stripQuantFsT2 []	t

stripQuantFsT2   vks t
 = case t of
 	T.TFetters fs x	-> (vks, fs, x)
	_		-> (vks, [], t)

hasFetter v fs
 	=  (not $ isNil [v | T.FLet  (T.TVar k v') eff <- fs, v == v'])




-----
toCoreT:: T.Type -> C.Type
toCoreT	   t
 = case t of
	T.TForall vs t
	 -> let vs'	= Var.sortForallVars 
			$ map fst vs

		vsKinds	= map	(\v -> ( v
				       , C.TKind $ toCoreK $ fromJust $ lookup v vs))
				vs'

	   in	foldl (\t (v, k) -> C.TForall v k t)
	   		(toCoreT t)
			(reverse vsKinds)
	   
	T.TFetters fs t
	 -> let	(fsLet, fsRest)	= partition ((=@=) T.FLet{}) fs
		
		vts		= [ (v, toCoreT t) | T.FLet (T.TVar k v) t	<- fsLet]

	    in	C.makeTWhere (addContexts (map toCoreF fsRest) (toCoreT t)) vts
	
	T.TSum k ts		-> C.TSum (toCoreK k) (map toCoreT ts)

	T.TMask k t1 t2		-> C.TMask (toCoreK k) (toCoreT t1) (toCoreT t2)

	T.TVar k v		-> C.TVar (toCoreK k) v 

	T.TBot k		-> C.TBot (toCoreK k)

	T.TTop k		-> C.TTop (toCoreK k)

	-- data
	T.TData v ts		-> C.TData v (map toCoreT ts)
	T.TFun t1 t2 eff clo	-> C.TFunEC (toCoreT t1) (toCoreT t2) (toCoreT eff) (toCoreT clo)
	
	-- effect
	T.TEffect v ts		-> C.TEffect v (map toCoreT ts)
	
	-- closure
	T.TFree v t		-> C.TFree v (toCoreT t)
	T.TTag v		-> C.TTag  v
	
	-- wildcards	
	T.TWild k		-> C.TWild (toCoreK k)

	T.TNode _ t		-> toCoreT t

	_ -> panic stage $ "toCoreT: failed to convert " ++ show t


-----
toCoreK :: T.Kind -> C.Kind
toCoreK k
 = case k of
	T.KNil			-> C.KNil
	T.KData			-> C.KData
	T.KRegion		-> C.KRegion
	T.KEffect		-> C.KEffect
	T.KClosure		-> C.KClosure
	T.KFetter		-> C.KClass
	T.KFun k1 k2		-> C.KFun (toCoreK k1) (toCoreK k2)
	


-----
toCoreF :: T.Fetter -> C.Class
toCoreF	   f
 = case f of
	T.FConstraint v ts		-> C.TClass v (map toCoreT ts)
	
	_ -> panic stage
		$ "toCoreF: cannot convert " % f % "\n"
		% "    f = " % show f % "\n"



addContexts :: [C.Class] -> C.Type -> C.Type
addContexts []	  t	= t
addContexts (f:fs) t
 = case f of
 	C.TClass v ts	-> C.TContext f (addContexts fs t)
	_		-> C.TContext C.TNil (addContexts fs t)
--	_ -> panic stage $ "addContexts: no match for " ++ show f	    
	   





