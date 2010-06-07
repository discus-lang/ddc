
module Core.Util.Pack
	( packT 
	, packK
	, inlineTWheresT
	, inlineTWheresMapT )
where
import Core.Plate.FreeVars
import Util
import Util.Graph.Deps
import Type.Util
import DDC.Main.Error
import DDC.Type
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Core.Util.Pack"

-- | Pack a type into standard form.
packT :: Type -> Type
packT tt	= {-# SCC "packT" #-} packT1 tt
 
-- | Do one round of packing
packT1 :: Type -> Type
packT1 tt 
 = case tt of
	-- push foralls under closure tags
	TForall BNil k1 t2
	 -> TForall BNil (packK k1) (packT1 t2)


	TForall v1 k1 tBody
	 -> let tBody'	= packT1 tBody
	    in	case takeTFree tBody' of
	 	 Just (v2, t2)	-> makeTFree v2 (TForall v1 k1 t2)
		 _		-> TForall v1 k1 tBody'
		
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

	-- Crush effects along the way
	TApp t1@(TCon (TyConEffect tyCon k)) t2
	 -> let t2'	= packT1 t2
	 
	 	result
			-- Head Read
	 		| TyConEffectHeadRead	<- tyCon
			= case takeTData t2' of
				Just (vD, k, (TVar kRegion r : ts)) 
					-> TApp tRead (TVar kRegion r)
				
				Just (vD, k, [])
					-> tPure

				Nothing	-> TApp t1 t2'
									
			-- Deep Read
			| TyConEffectDeepRead	<- tyCon
			= case takeTData t2' of
				Just (vD, k, ts)
				 -> let (tRs, tDs) = unzip $ map slurpVarsRD ts
				    in  makeTSum kEffect
						(  [TApp tRead t	| t <- concat tRs]
						++ [TApp tDeepRead t	| t <- concat tDs] )

				Nothing	-> TApp t1 t2'
				
			-- Deep Write
			| TyConEffectDeepWrite <- tyCon
			= case takeTData t2' of
				Just (vD, k, ts)
				 -> let (tRs, tDs) = unzip $ map slurpVarsRD ts
				    in  makeTSum kEffect
						(  [TApp tWrite t	| t <- concat tRs]
						++ [TApp tDeepWrite t	| t <- concat tDs] )

				Nothing	-> TApp t1 t2'
	
			-- some other effect
			| otherwise
			= TApp t1 t2'

	    in	result
	    
	
	-- Crush witnesses along the way
	TApp t1@(TCon (TyConWitness tyCon k)) t2
	 -> let t2'	= packT1 t2
	 	
		result
			-- HeadLazy
	 		| TyConWitnessMkHeadLazy <- tyCon
			, Just (vD, k, (TVar kRegion r : ts))	<- takeTData t2'
			= TApp tMkLazy (TVar kRegion r)

			-- DeepConst
			| TyConWitnessMkDeepConst <- tyCon
			, Just _		<- takeTData t2'
			, (rs, ds)		<- slurpVarsRD t2'
			= let 	ts       =  map (\r -> TApp tMkConst     r) rs
				         ++ map (\d -> TApp tMkDeepConst d) ds

				ks 	=  map kindOfType ts
			  in 	makeTSum (KSum ks) ts
			
			-- DeepMutable
			| TyConWitnessMkDeepMutable <- tyCon
			, Just _		<- takeTData t2'
			, (rs, ds)		<- slurpVarsRD t2'
			= let	ts	=  map (\r -> TApp tMkMutable     r) rs
				   	++ map (\d -> TApp tMkDeepMutable d) ds
				
				ks 	= map kindOfType ts
			  in	makeTSum (KSum ks) ts
			
			-- leave it alone.
			| otherwise
			= TApp t1 t2'
	    in	result
	
	TApp{}
	 | Just (v1, t2)	<- takeTFree tt
	 , t2'			<- packT1 t2
	 -> let	result
		 | Just (v2, t2X)	<- takeTFree t2'
		 = makeTFree v1 t2X
		
		 | TSum k ts		<- t2'
		 , k == kClosure
		 = TSum kClosure (map (makeTFree v1) ts)
		
		 | otherwise	
		 = makeTFree v1 t2'
		
	   in result

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
	TVar k (UVar v)		-> tt
	TVar k (UMore v t)	-> TVar k $ UMore v (packT1 t)
	TVar _ UIndex{}		-> tt

	TCon{}			-> tt
	
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
	= makeKSum
	 	$  map (\r -> KApp kMutable     r) rs
		++ map (\d -> KApp kDeepMutable d) ds
				
	-- crush DeepConst on the way
	| KApp kc t1	<- kk
	, Just _		<- takeTData t1
	, kc == kDeepConst
	, (rs, ds)		<- slurpVarsRD t1
	= makeKSum
		$  map (\r -> KApp kConst     r) rs
		++ map (\d -> KApp kDeepConst d) ds

	-- Join purification witness kinds
	| KSum ks		<- kk
	, Just tsEffs		<- sequence $ map takeKClass_pure ks
	, ksEffs		<- map kindOfType tsEffs
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
				(Map.fromList [(v, t) | FWhere (TVar _ (UVar v)) t <- fsWhere])
				sub
				
				
	    	tFlat	= inlineTWheresMapT 
				sub'
				block
				t1

	    in	makeTFetters tFlat fsMore

	TSum     k ts		-> TSum  k 	(map down ts)
	    
	TVar k (UVar v)
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

	TVar k (UMore v tMore)
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
		
    	TCon{}			-> tt
    
	TApp t1 t2		-> TApp (down t1) (down t2)
 		    

-- | Restrict the list of FWhere fetters to ones which are 
--	reachable from this type. Also erase x = Bot fetters.
--
restrictBinds :: Type -> [Fetter] -> [Fetter]
restrictBinds tt ls
 = let	splitFetter ff
  	 = case ff of
	 	FWhere (TVar k (UVar v)) t	-> (v, t)
		FMore  (TVar k (UVar v)) t	-> (v, t)

 	reachFLetsMap
 		= Map.fromList
		$ [(t, freeVars tLet)	
 			| (t, tLet)	<- map splitFetter ls]
 
 	vsSeed		= freeVars tt

	vsReachable	= vsSeed `Set.union` graphReachableS reachFLetsMap vsSeed
	
	varOfFetter f
	 = case f of
		FWhere (TVar k (UVar v)) _	-> v
		FMore  (TVar k (UVar v)) _	-> v
	 
   in	filter	(\f -> Set.member (varOfFetter f) vsReachable)
   		ls

