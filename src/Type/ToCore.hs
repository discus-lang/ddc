
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
import Shared.VarPrim
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
import qualified Core.ReconKind	as C

import qualified Data.Map	as Map
import Data.Map			(Map)

-----
stage	= "Type.ToCore"

-- Convertion of source types to core representation.
--	- :> constraints on type variables are carried directly in the variable

-- Type ---------------------------------------------------------------------------------------------
-- | Convert this type to core representation.
toCoreB :: T.Bind -> C.Bind
toCoreB bb
 = case bb of
 	T.BVar  v	-> C.BVar v
	T.BMore v t	-> C.BMore v (toCoreT t)


toCoreT:: T.Type -> C.Type
toCoreT tt	= toCoreT' Map.empty tt

toCoreT' :: Map Var T.Type -> T.Type -> C.Type
toCoreT' table tt
 = let down x	= toCoreT' table x
   in case tt of
	
	-- Add :> constraints on type variables directly to the quantifer.
	T.TForall b k tQuant
	 -> let result
	 		| (bks, T.TFetters tBody fs)	<- T.slurpTForall tt
			= let	
				(fsMore, fsRest) = partition ((=@=) T.FMore{}) fs
				vtsMore		 = Map.fromList
							[(v, t)	| T.FMore (T.TVar _ v) t <- fsMore]

				makeBK b
				 = let	v	= T.varOfBind b
				 	b'	= case Map.lookup v vtsMore of
							Nothing	-> C.BVar v
							Just t	-> C.BMore v (toCoreT t)
					k	= toCoreK $ fromJust $ lookup b bks
				   in	(b', k)
				   
				bks'		= map (makeBK . fst) bks
				
				table'	= Map.union table vtsMore

	   		in	foldl	(\t (b, k) -> C.TForall b k t)
			   		(toCoreT' table' $ T.TFetters tBody fsRest)
					(reverse bks')

			| (bks, tBody)			<- T.slurpTForall tt
			= foldl (\t (b, k) -> C.TForall (toCoreB b) (toCoreK k) t)
				(toCoreT' table tBody)
				(reverse bks)

	    in	result
	   
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

	-- TODO: don't assume there are no kind problems in the constraint
	T.FConstraint v ts		
	 -> let	ts'	= map toCoreT ts
		Just t	= C.buildWitnessOfClass (C.KClass (tyClassOfVar v) ts')
	    in	t
	    
	_ -> panic stage
		$ "toCoreF: cannot convert " % f % "\n"
		% "    f = " % show f % "\n"


-- Detect type classes which have a special meaning to the compiler.
tyClassOfVar :: Var -> C.TyClass
tyClassOfVar v
 = case Map.lookup v tyClassPrim of
 	Just tc	-> tc
	_	-> C.TyClass v

tyClassPrim
 = Map.fromList
 	[ (primConst, 		C.TyClassConst)
 	, (primConstT,		C.TyClassConstT)
	, (primMutable,		C.TyClassMutable)
	, (primMutableT,	C.TyClassMutableT)
	, (primLazy,		C.TyClassLazy)
	, (primLazyH,		C.TyClassLazyH)
	, (primDirect,		C.TyClassDirect)
	, (primPure,		C.TyClassPure) 
	, (primEmpty,		C.TyClassEmpty) ]


addContexts :: [C.Class] -> C.Type -> C.Type
addContexts []	   t	= t
addContexts (f:fs) t	
	| Just k	<- C.kindOfType f
	= C.TContext k (addContexts fs t)
		
   





