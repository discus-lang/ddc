
-- | Convertion of types to Core IR representation.
--	 We use the same data type, but the representation is slightly different.
--
module Type.ToCore
	( toCoreT
	, toCoreK
	, toCoreF)
where
import Util
import Type.Util
import Shared.VarPrim
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Var
import qualified Data.Map	as Map
import qualified Debug.Trace

stage		= "Type.ToCore"
debug		= False
trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x

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
	 		| (bks, TFetters tBody fs)	<- takeTForall tt
			= let	
				(fsMore, fsRest) = partition isFMore fs
				vtsMore		 = Map.fromList
							[(v, t)	| FMore (TVar _ (UVar v)) t <- fsMore]

				makeBK b
				 = let	Just v	= takeVarOfBind b
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

			| (bks, tBody)		<- takeTForall tt
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
					| FWhere (TVar k (UVar v)) t	<- fsLet]

		vtsMore	= Map.fromList
			$ [ (v, t)	| FMore  (TVar _ (UVar v)) t	<- fsMore]

		table'	= Map.union table vtsMore
		t'	= toCoreT' table' t

	    in	makeTWhere (addContextsUnderForalls (map toCoreF fsRest1) t') vtsLet
	
	TConstrain t crs
	 -> toCoreT' table $ toFetterFormT tt
	
	TSum k ts		-> TSum (toCoreK k) (map down ts)

	-- attach :> constraints directly to variables
	TVar k (UVar v)		
	 -> case Map.lookup v table of
	 	Nothing		-> TVar (toCoreK k) $ UVar v
		Just tMore	-> TVar (toCoreK k) $ UMore v (toCoreT' (Map.delete v table) tMore)

	TCon tyCon	-> TCon (toCoreTyCon tyCon)

	-- data
	TApp t1 t2
	 | Just (v, k, ts)	<- takeTData tt
	 -> let tyCon	= TyConData 
		 		{ tyConName		= v
				, tyConDataKind	= k }
	    in  makeTApp (TCon tyCon) (map down ts)

	 | Just (t11, t12, eff, clo) <- takeTFun tt
	 -> makeTFun (down t11) (down t12) (down eff) (down clo)
	

	 -- We don't need TDanger types in the core language, 
	 -- but we still need to remember value types or regions that are a part of the closure.
	 -- 
	 -- NOTE: Effect variables can be dangerous, so we can get terms like  x : %r1 $> !e1
	 --       from the inferencer, but we can't form a term like (x : !e1) in the core 
	 --       language because there isn't a corresponding TFree constructor of kind (! -> $).
	 -- 
	 -- v : t211 $> t212
	 | Just (v,   t21)	<- takeTFree tt
	 , Just (t211, t212)	<- takeTDanger t2
	 , k211			<- kindOfType t211
	 , k212			<- kindOfType t212
	 -> makeTSum kClosure
	 	[ if hasKindTRC t211 then makeTFreeBot v (down t211) else tBot kClosure
		, if hasKindTRC t212 then makeTFreeBot v (down t212) else tBot kClosure]
	
	 | otherwise
	 -> TApp (down t1) (down t2)
			
	_ 	-> panic stage 
			$ "toCoreT: failed to convert " % tt 	% "\n"
			% "    tt = " % show tt			% "\n"

hasKindTRC :: Type -> Bool
hasKindTRC tt
 = let	k	= kindOfType tt
   in	isRegionKind k || isValueKind k || isClosureKind k


	
-- TyCon -------------------------------------------------------------------------------------------
toCoreTyCon :: TyCon -> TyCon
toCoreTyCon tt
 = case tt of
 	TyConFun 	 -> TyConFun

	TyConData v k
	 -> TyConData v (toCoreK k)

	TyConEffect tc k
	 -> TyConEffect tc (toCoreK k)
	
	TyConClosure tc k
	 -> TyConClosure tc (toCoreK k)
	

-- Kind ---------------------------------------------------------------------------------------------
toCoreK :: Kind -> Kind
toCoreK k	= k


-- Fetter ------------------------------------------------------------------------------------------
toCoreF :: Fetter -> Kind
toCoreF	f
 = case f of
	FConstraint v tsArg		
	 -> let	tsArg'		= map toCoreT tsArg
		ksArg		= map kindOfType tsArg
		kClass		= makeKindOfTypeClass v ksArg
		kWitness	= makeKApps kClass tsArg'
	    in	trace (vcat
			[ ppr "toCoreF"
			, "    fetter: " % f
			, "    kind:   " % kWitness]) 
			kWitness
	    
	_ -> panic stage
		$ "toCoreF: cannot convert " % f % "\n"
		% "    f = " % show f % "\n"


-- | Make the kind of some type class constraint.
makeKindOfTypeClass 
	:: Var 		-- ^ Name of type class.
	-> [Kind] 	-- ^ Kinds of arguments to type class.
	-> Kind
	
makeKindOfTypeClass vClass ksArgs
	
	-- Detect type classes that have a special meaning to the compiler.
	| Just kClass	<- Map.lookup vClass primClassKinds
	= kClass

	-- If its some user defined class then base the superkind on the
	--	kinds of the class arguments.
	| otherwise
	= let super	= makeSuperFun ksArgs SProp
	  in  KCon (KiConVar vClass) super



primClassKinds
 = Map.fromList
 	[ (primConst, 		kConst)
 	, (primConstT,		kDeepConst)
	, (primMutable,		kMutable)
	, (primMutableT,	kDeepMutable)
	, (primLazy,		kLazy)
	, (primLazyH,		kHeadLazy)
	, (primDirect,		kDirect)
	, (primPure,		kPure) 
	, (primEmpty,		kEmpty) ]


		
   