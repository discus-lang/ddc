
-- | Convertion of types to Core IR representation.
module Type.ToCore
	( toCoreT
	, toCoreK
	, toCoreF)
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

-----
stage	= "Type.ToCore"


-- | Convert this type to core representation.
toCoreT:: T.Type -> C.Type
toCoreT	   tt
 = case tt of
	-- Check for constraints on quantifiers
	--	If any are present they'll be in the list of fetters
	T.TForall vsk (T.TFetters fs t)
	 -> let	(fsMore, fsRest)	
			= partition ((=@=) T.FMore{}) fs

		vsMore	= [(v, t)	| T.FMore (T.TVar _ v) t	<- fsMore]

		vs'	= Var.sortForallVars 
			$ map fst vsk

		bsKinds	= map	(\v -> ( case lookup v vsMore of
						Nothing	-> C.BVar v
						Just t	-> C.BMore v (toCoreT t)

				       , toCoreK $ fromJust $ lookup v vsk))
				vs'

		t'	= toCoreT (T.TFetters fsRest t)

	   in	foldl (\t (b, k) -> C.TForall b k t)
	   		t'
			(reverse bsKinds)

	-- Forall with no fetters underneath
	T.TForall vs t
	 -> let vs'	= Var.sortForallVars 
			$ map fst vs

		vsKinds	= map	(\v -> ( v
				       , toCoreK $ fromJust $ lookup v vs))
				vs'

		t'	= toCoreT t

	   in	foldl (\t (v, k) -> C.TForall (C.BVar v) k t)
	   		t'
			(reverse vsKinds)
	   
	T.TFetters fs t
	 -> let	
	 	-- separate out all the FLet bindings, we'll add these as a TWhere to the core type
	 	(fsLet, fsRest1)	
	 		= partition ((=@=) T.FLet{}) fs
				
		vts		= [ (v, toCoreT t) | T.FLet (T.TVar k v) t	<- fsLet]

	    in	C.makeTWhere (addContexts (map toCoreF fsRest1) (toCoreT t)) vts
	
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

	_ -> panic stage $ "toCoreT: failed to convert " ++ show tt

-----
toCoreK :: T.Kind -> C.Kind
toCoreK k
 = case k of
	T.KNil			-> C.KNil
	T.KData			-> C.KData
	T.KRegion		-> C.KRegion
	T.KEffect		-> C.KEffect
	T.KClosure		-> C.KClosure
	T.KFun k1 k2		-> C.KFun (toCoreK k1) (toCoreK k2)
	
	_ -> panic stage
		$ "toCoreK: cannot convert " % k % "\n"


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
 	C.TClass v ts	-> C.TContext (C.KClass v ts) (addContexts fs t)
--	_		-> C.TContext C.KNil          (addContexts fs t)
	_ -> panic stage $ "addContexts: no match for " ++ show f	    
	   





