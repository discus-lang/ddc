
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
import qualified Type.Util	as T
import qualified Type.Pretty	as T

import qualified Core.Exp 	as C
import qualified Core.Util	as C

import qualified Data.Map	as Map
import Data.Map			(Map)

-----
stage	= "Type.ToCore"

-- Convertion of source types to core representation.
--	- :> constraints on type variables are carried directly in the variable

-- Type ---------------------------------------------------------------------------------------------
-- | Convert this type to core representation.
toCoreT:: T.Type -> C.Type
toCoreT tt	= toCoreT' Map.empty tt

toCoreT' :: Map Var T.Type -> T.Type -> C.Type
toCoreT' table tt
 = let down x	= toCoreT' table x
   in case tt of
	
	
	-- Add :> constraints on type variables directly to the quantifer.
	T.TForall vks (T.TFetters t fs)
	 -> let	(fsMore, fsRest)	
			= partition ((=@=) T.FMore{}) fs

		vtsMore	= Map.fromList
			$ [(v, t)	| T.FMore (T.TVar _ v) t	<- fsMore]

		bsKinds	= map	(\v -> (case Map.lookup v vtsMore of
						Nothing	-> C.BVar v
						Just t	-> C.BMore v (toCoreT t)

				       , toCoreK $ fromJust $ lookup v vks))
			$ map fst vks
		
		table'	= Map.union table vtsMore

	   in	foldl	(\t (b, k) -> C.TForall b k t)
	   		(toCoreT' table' $ T.TFetters t fsRest)
			(reverse bsKinds)

	-- Forall with no fetters underneath
	T.TForall vks t
	 -> let vsKinds	= map	(\v -> ( v
				       , toCoreK $ fromJust $ lookup v vks))
			$ map fst vks

	   in	foldl	(\t (v, k) -> C.TForall (C.BVar v) k t)
	   		(down t)
			(reverse vsKinds)
	   
	T.TFetters t fs
	 -> let	
	 	-- separate out all the FLet bindings, we'll add these as a TWhere to the core type
	 	([fsLet, fsMore], fsRest1)
	 		= partitionFs [(=@=) T.FLet{}, (=@=) T.FMore{}] fs
				
		vtsLet	= [ (v, toCoreT t) 	
					| T.FLet (T.TVar k v) t		<- fsLet]

		vtsMore	= Map.fromList
			$ [ (v, t)	| T.FMore (T.TVar _ v) t	<- fsMore]

		table'	= Map.union table vtsMore
		t'	= toCoreT' table' t

	    in	C.makeTWhere (addContexts (map toCoreF fsRest1) t') vtsLet
	
	T.TSum k ts		-> C.TSum (toCoreK k) (map down ts)

	T.TMask k t1 t2		-> C.TMask (toCoreK k) (down t1) (down t2)

	-- attach :> constraints directly to variables
	T.TVar k v		
	 -> case Map.lookup v table of
	 	Nothing		-> C.TVar     (toCoreK k) v 
		Just tMore	-> C.TVarMore (toCoreK k) v (toCoreT' (Map.delete v table) tMore)

	T.TBot k		-> C.TBot (toCoreK k)

	T.TTop k		-> C.TTop (toCoreK k)

	T.TApp t1 t2		-> C.TApp (down t1) (down t2)
	T.TCon tyCon		-> C.TCon (toCoreTyCon tyCon)

	-- data
	T.TData k v ts		
	 -> let tyCon	= T.TyConData 
		 		{ T.tyConName	= v
				, T.tyConKind	= k }
	   in  down $ T.makeTApp (T.TCon tyCon : ts)


	T.TFun t1 t2 eff clo	-> C.makeTFun (down t1) (down t2) (down eff) (down clo)
	
	-- effect
	T.TEffect v ts		-> C.TEffect v (map down ts)
	
	-- closure
	T.TFree v (T.TDanger t1 t2)
				-> C.makeTSum C.KClosure
					[ C.TFree v (down t1)
					, C.TFree v (down t2)]
				
				
	T.TFree v t		-> C.TFree v (down t)
	T.TTag v		-> C.TTag  v
	
	-- wildcards	
	T.TWild k		-> C.TWild (toCoreK k)

	_ 	-> panic stage 
			$ "toCoreT: failed to convert " % tt 	% "\n"
			% "    tt = " % show tt			% "\n"
	
-- TyCon -------------------------------------------------------------------------------------------
toCoreTyCon :: T.TyCon -> C.TyCon
toCoreTyCon tt
 = case tt of
 	T.TyConFun k 
	 -> C.TyConFun

	T.TyConData v k
	 -> C.TyConData v (toCoreK k)

-- Kind ---------------------------------------------------------------------------------------------
toCoreK :: T.Kind -> C.Kind
toCoreK k
 = case k of
	T.KNil			-> C.KNil
	T.KValue		-> C.KValue
	T.KRegion		-> C.KRegion
	T.KEffect		-> C.KEffect
	T.KClosure		-> C.KClosure
	T.KFun k1 k2		-> C.KFun (toCoreK k1) (toCoreK k2)
	
	_ -> panic stage
		$ "toCoreK: cannot convert " % k % "\n"


-- Fetter ------------------------------------------------------------------------------------------
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
	   





