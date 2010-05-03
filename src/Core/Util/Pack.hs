
module Core.Util.Pack
	( packT 
	, packK
	, flattenT 
	, inlineTWheresT
	, inlineTWheresMapT )
where
import Core.Plate.FreeVars
import Type.Exp
import Type.Builtin
import Shared.VarPrim
import Util
import Util.Graph.Deps
import DDC.Main.Error
import Type.Util		hiding (flattenT)
import qualified Data.Map	as Map
import qualified Data.Set	as Set

-----
stage	= "Core.Util.Pack"


-- | Pack a type into standard form.
packT :: Type -> Type
packT tt	= {-# SCC "packT" #-} packT1 tt

-- | Flatten a type so that all where bindings are inlined
flattenT :: Type -> Type
flattenT tt
 = {-# SCC "flattenT" #-}
    packT $ inlineTWheresT tt

 
-- | Do one round of packing
packT1 :: Type -> Type
packT1 tt 
 = case tt of
	-- push foralls under closure tags
	TForall v1 k1 tBody
	 -> case packT1 tBody of
	 	TFree v2 t2	-> TFree v2 (TForall v1 k1 t2)
		tBody'		-> TForall v1 k1 tBody'
		
	 
	TContext k1 t2
	 -> TContext (packK k1) (packT1 t2)

	TFetters t1 fs
	 -> let t1'	= packT1 t1
	 	fs'	= restrictBinds t1' fs

		-- short out single (v :- v = ..) fetter
	    	result	
			| TVar k v1			<- t1'
			, [FWhere (TVar _ v2) t2]	<- fs'
			, v1 == v2
			= t2
			
			| otherwise
			= makeTFetters t1' fs'
			
	   in	result
	
	-- Crush witnesses along the way
	TApp t1@(TCon (TyConWitness tyCon k)) t2
	 -> let t2'	= packT1 t2
	 	
		result
			-- crush LazyH on the way
	 		| tyCon == TyConWitnessMkHeadLazy
			, Just (vD, k, (TVar kRegion r : ts))	<- takeTData t2'
			= TApp tMkLazy (TVar kRegion r)

			-- crush ConstT on the way
			| tyCon == TyConWitnessMkDeepConst 
			, Just _		<- takeTData t2'
			, (rs, ds)		<- slurpVarsRD t2'
			= makeTWitJoin 
			    	$ (   map (\r -> TApp tMkConst     r) rs
				   ++ map (\d -> TApp tMkDeepConst d) ds)

			-- crush MutableT on the way
			| tyCon == TyConWitnessMkDeepMutable
			, Just _		<- takeTData t2'
			, (rs, ds)		<- slurpVarsRD t2'
			= makeTWitJoin 
			    	$ (   map (\r -> TApp tMkMutable     r) rs
				   ++ map (\d -> TApp tMkDeepMutable d) ds)

			| otherwise
			= TApp t1 t2'
	    in	result
	

	TApp t1 t2
	 -> let	t1'	= packT1 t1
	 	t2'	= packT1 t2
		
		-- lift fetters above args
		(t1_lift, fss1)	= slurpTFetters t1'
		(t2_lift, fss2) = slurpTFetters t2'

	    in	makeTFetters (TApp t1_lift t2_lift) (fss1 ++ fss2)

	    
	-- sums
	TSum k ts
	 -> makeTSum k $ nub $ map packT1 ts

	-- vars
	TVar k v	-> tt
	TVarMore k v t	-> TVarMore k v (packT1 t)
	TIndex{}	-> tt

	TCon{}		-> tt
	TBot{}		-> tt
	TTop{} 		-> tt
	 
	-- effect
	-- crush compound effects along the way
	TEffect v [t1]
	 -> let t1'	= packT1 t1
	 
	 	result
			-- ReadH 
	 		| v == primReadH
			= case takeTData t1' of
				Just (vD, k, (TVar kRegion r : ts)) 
					-> TEffect primRead [TVar kRegion r]
				
				Just (vD, k, [])
					-> tPure

				Nothing	-> TEffect v [t1']
					
			-- ReadT
			| v == primReadT
			= case takeTData t1' of
				Just (vD, k, ts)
				 -> let (tRs, tDs) = unzip $ map slurpVarsRD ts
				    in  makeTSum kEffect
						(  [TEffect primRead  [t]	| t <- concat tRs]
						++ [TEffect primReadT [t]	| t <- concat tDs] )

				Nothing	-> TEffect v [t1']
				
			-- WriteT
			| v == primWriteT
			= case takeTData t1' of
				Just (vD, k, ts)
				 -> let (tRs, tDs) = unzip $ map slurpVarsRD ts
				    in  makeTSum kEffect
						(  [TEffect primWrite  [t]	| t <- concat tRs]
						++ [TEffect primWriteT [t]	| t <- concat tDs] )

				Nothing	-> TEffect v [t1']
	
			-- some other effect
			| otherwise
			= TEffect v [t1']

	    in	result
	    
	TEffect v ts
	 -> TEffect v (map packT1 ts)
	    
	-- closure
	TFree v1 t2
	 -> let	t2'	= packT1 t2
	    in	case t2' of
	    		TBot k    | k == kClosure	-> tEmpty
			TFree v2 t2X			-> TFree v1 t2X
			TSum k ts | k == kClosure	-> TSum kClosure (map (TFree v1) ts)
			_				-> TFree v1 t2'
						
	TWitJoin ts
	 -> makeTWitJoin (map packT1 ts)
	
	_ -> panic stage
		$ "packT: no match for " % tt % "\n"
		% "  tt = " % show tt % "\n"


slurpTFetters (TFetters t fs)	= (t, fs)
slurpTFetters tt		= (tt, [])


-- Kind Packing ------------------------------------------------------------------------------------
-- | Pack a kind into standard form.
packK :: Kind -> Kind
packK kk
 = let kk'	= packK1 kk
   in  if kk == kk'
   	then kk'
	else packK1 kk'


-- | Do one round of packing on this kind
packK1 :: Kind -> Kind
packK1 kk
	-- crush HeadLazy on the way
	| KApp kc t1	<- kk
	, Just (vD, k, TVar kR r : ts)	<- takeTData t1
	, kR == kRegion
	, kc == kHeadLazy
	= KApp kLazy (TVar kR r)

	-- crush DeepMutable on the way
	| KApp kc t1	<- kk
	, Just _		<- takeTData t1
	, kc == kDeepMutable
	, (rs, ds)		<- slurpVarsRD t1
	= makeKWitJoin 
		$  map (\r -> KApp kMutable     r) rs
		++ map (\d -> KApp kDeepMutable d) ds
	
	-- crush DeepConst on the way
	| KApp kc t1	<- kk
	, Just _		<- takeTData t1
	, kc == kDeepConst
	, (rs, ds)		<- slurpVarsRD t1
	= makeKWitJoin 
		$  map (\r -> KApp kConst     r) rs
		++ map (\d -> KApp kDeepConst d) ds

	-- Join purification witness kinds
	| KWitJoin ks		<- kk
	, Just tsEffs		<- sequence $ map takeKClass_pure ks
	, Just ksEffs		<- sequence $ map kindOfType tsEffs
	, and $ map (== kEffect) ksEffs
	= KApp kPure (TSum kEffect tsEffs)

	-- pack types inside witness kinds
	| KApp kc t	<- kk
	= KApp kc (packT t)
	    
	| otherwise
	= kk


-- | Type the type args from a pure
takeKClass_pure :: Kind -> Maybe Type
takeKClass_pure kk
 = case kk of
 	KApp k t
	 | k == kPure		-> Just t
	_ 			-> Nothing
	

-- Inline ------------------------------------------------------------------------------------------
-- | Inline all TLet expressions in this type.
inlineTWheresT :: Type -> Type
inlineTWheresT tt
 = inlineTWheresMapT Map.empty Set.empty tt

inlineTWheresMapT sub block tt
 = let down	= inlineTWheresMapT sub block
   in  case tt of
 	TNil			-> tt
	
	TForall v k t		-> TForall v k (down t)
	    
	TFetters t1 fs
	 -> let	([fsWhere, fsMore], [])
	 		= partitionFs [isFWhere, isFMore] fs
		
		sub'	= Map.union 
				(Map.fromList [(v, t) | FWhere (TVar _ v) t <- fsWhere])
				sub
				
				
	    	tFlat	= inlineTWheresMapT 
				sub'
				block
				t1

	    in	makeTFetters tFlat fsMore


	TContext l t		-> TContext l 	(down t)
	TSum     k ts		-> TSum  k 	(map down ts)
	    
	TVar k v	
	 -- If this var is in our block set then we're trying to recursively
	 --	substitute for it.. bail out now or we'll loop forever.
	 |  Set.member v block
	 -> tt

	 -- Lookup the var and add it to the block list so we can detect loops
	 --	in the type.
	 | otherwise
	 -> case Map.lookup v sub of
	 	Just t	-> inlineTWheresMapT sub (Set.insert v block) t
		_	-> tt

	TVarMore k v tMore
	 -- If this var is in our block set then we're trying to recursively
	 --	substitute for it.. bail out now or we'll loop forever.
	 |  Set.member v block
	 -> tt

	 -- Lookup the var and add it to the block list so we can detect loops
	 --	in the type.
	 | otherwise
	 -> case Map.lookup v sub of
	 	Just t	-> inlineTWheresMapT sub (Set.insert v block) t
		_	-> tt
		
    	TTop{}			-> tt
	TBot{}			-> tt
    	TCon{}			-> tt
    
	TApp t1 t2		-> TApp (down t1) (down t2)

	-- effect
	TEffect  v ts		-> TEffect v (map down ts)
 	
	-- closure
	TFree v t		-> TFree v (down t)
	    

-- | Restrict the list of FWhere fetters to ones which are 
--	reachable from this type. Also erase x = Bot fetters.
--
restrictBinds :: Type -> [Fetter] -> [Fetter]
restrictBinds tt ls
 = let	splitFetter ff
  	 = case ff of
	 	FWhere (TVar k v) t	-> (v, t)
		FMore  (TVar k v) t	-> (v, t)

 	reachFLetsMap
 		= Map.fromList
		$ [(t, freeVars tLet)	
 			| (t, tLet)	<- map splitFetter ls]
 
 	vsSeed		= freeVars tt

	vsReachable	= vsSeed `Set.union` graphReachableS reachFLetsMap vsSeed
	
	varOfFetter f
	 = case f of
		FWhere (TVar k v) _	-> v
		FMore  (TVar k v) _	-> v
	 
   in	filter	(\f -> Set.member (varOfFetter f) vsReachable)
   		ls

