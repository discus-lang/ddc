
-- | Convertion of types to Core IR representation.
--	 We use the same data type, but the representation is slightly different.
--
module Type.ToCore
	( toCoreT
	, toCoreK
	, toCoreF)
where
import Util
import Type.Exp	
import Type.Util
import Shared.VarPrim
import Shared.Error		(panic)
import Shared.Var		(Var)
import qualified Data.Map	as Map

-----
stage	= "Type.ToCore"


-- Bind -------------------------------------------------------------------------------------------
toCoreB :: Bind -> Bind
toCoreB bb
 = case bb of
 	BVar  v		-> BVar v
	BMore v t	-> BMore v (toCoreT t)


-- Type -------------------------------------------------------------------------------------------

-- | Convert this type to core representation.
toCoreT:: Type -> Type
toCoreT tt	
	= toCoreT' Map.empty tt


toCoreT' :: Map Var Type -> Type -> Type
toCoreT' table tt
 = let down x	= toCoreT' table x
   in case tt of
	
	-- Add :> constraints on type variables directly to the quantifer.
	TForall b k tQuant
	 -> let result
	 		| (bks, TFetters tBody fs)	<- slurpTForall tt
			= let	
				(fsMore, fsRest) = partition isFMore fs
				vtsMore		 = Map.fromList
							[(v, t)	| FMore (TVar _ v) t <- fsMore]

				makeBK b
				 = let	v	= varOfBind b
				 	b'	= case Map.lookup v vtsMore of
							Nothing	-> BVar v
							Just t	-> BMore v (toCoreT t)
					k	= toCoreK $ fromJust $ lookup b bks
				   in	(b', k)
				   
				bks'		= map (makeBK . fst) bks
				
				table'	= Map.union table vtsMore

	   		in	foldl	(\t (b, k) -> TForall b k t)
			   		(toCoreT' table' $ TFetters tBody fsRest)
					(reverse bks')

			| (bks, tBody)			<- slurpTForall tt
			= foldl (\t (b, k) -> TForall (toCoreB b) (toCoreK k) t)
				(toCoreT' table tBody)
				(reverse bks)

	    in	result
	   
	TFetters t fs
	 -> let	
	 	-- separate out all the FLet bindings, we'll add these as a TWhere to the core type
	 	([fsLet, fsMore], fsRest1)
	 		= partitionFs [isFWhere, isFMore] fs
				
		vtsLet	= [ (v, toCoreT t) 	
					| FWhere (TVar k v) t	<- fsLet]

		vtsMore	= Map.fromList
			$ [ (v, t)	| FMore  (TVar _ v) t	<- fsMore]

		table'	= Map.union table vtsMore
		t'	= toCoreT' table' t

	    in	makeTWhere (addContexts (map toCoreF fsRest1) t') vtsLet
	
	TConstrain t crs
	 -> toCoreT' table $ toFetterFormT tt
	
	TSum k ts		-> TSum (toCoreK k) (map down ts)

	-- attach :> constraints directly to variables
	TVar k v		
	 -> case Map.lookup v table of
	 	Nothing		-> TVar     (toCoreK k) v 
		Just tMore	-> TVarMore (toCoreK k) v (toCoreT' (Map.delete v table) tMore)

	TBot k		-> TBot (toCoreK k)

	TTop k		-> TTop (toCoreK k)

	TCon tyCon	-> TCon (toCoreTyCon tyCon)

	-- data
	TApp t1 t2
	 | Just (v, k, ts)	<- takeTData tt
	 -> let tyCon	= TyConData 
		 		{ tyConName		= v
				, tyConDataKind	= k }
	    in  makeTApp (TCon tyCon : map down ts)

	 | Just (t11, t12, eff, clo) <- takeTFun tt
	 -> makeTFun (down t11) (down t12) (down eff) (down clo)
	
	 | otherwise
	 -> TApp (down t1) (down t2)
	
	-- effect
	TEffect v ts		-> TEffect v (map down ts)
	
	-- closure
	TFree v (TDanger t1 t2)
		-> makeTSum kClosure
			[ TFree v (down t1)
			, TFree v (down t2)]
				
				
	TFree v t
		-> TFree v (down t)
	
	_ 	-> panic stage 
			$ "toCoreT: failed to convert " % tt 	% "\n"
			% "    tt = " % show tt			% "\n"
	
-- TyCon -------------------------------------------------------------------------------------------
toCoreTyCon :: TyCon -> TyCon
toCoreTyCon tt
 = case tt of
 	TyConFun 	 -> TyConFun

	TyConData v k
	 -> TyConData v (toCoreK k)

-- Kind ---------------------------------------------------------------------------------------------
toCoreK :: Kind -> Kind
toCoreK k	= k

-- Fetter ------------------------------------------------------------------------------------------
toCoreF :: Fetter -> Witness
toCoreF	f
 = case f of

	-- TODO: don't assume there are no kind problems in the constraint
	FConstraint v ts		
	 -> let	ts'	= map toCoreT ts
		Just t	= inventWitnessOfClass (KClass (tyClassOfVar v) ts')
	    in	t
	    
	_ -> panic stage
		$ "toCoreF: cannot convert " % f % "\n"
		% "    f = " % show f % "\n"


-- Detect type classes which have a special meaning to the compiler.
tyClassOfVar :: Var -> TyClass
tyClassOfVar v
 = case Map.lookup v tyClassPrim of
 	Just tc	-> tc
	_	-> TyClass v

tyClassPrim
 = Map.fromList
 	[ (primConst, 		TyClassConst)
 	, (primConstT,		TyClassConstT)
	, (primMutable,		TyClassMutable)
	, (primMutableT,	TyClassMutableT)
	, (primLazy,		TyClassLazy)
	, (primLazyH,		TyClassLazyH)
	, (primDirect,		TyClassDirect)
	, (primPure,		TyClassPure) 
	, (primEmpty,		TyClassEmpty) ]


addContexts :: [Witness] -> Type -> Type
addContexts []	   t	= t
addContexts (f:fs) t	
	| Just k	<- kindOfType f
	= TContext k (addContexts fs t)
		
   





