
module Core.Util.Pack
	( packT 
	, packK
	, flattenT 
	, inlineTWheresT
	, inlineTWheresMapT )
where
	
import Core.Exp
import Core.Plate.Trans
import Core.Plate.FreeVars
import Core.Util.Bits

import Type.Util		hiding (flattenT)

import Shared.Error
import Shared.VarUtil
import Shared.VarPrim
import Util
import Util.Graph.Deps

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set

-----
stage	= "Core.Util.Pack"


-- | Pack a type into standard form.
packT :: Type -> Type
packT tt
 = {-# SCC "packT" #-}
   let tt'	= packT1 tt
   in  if tt == tt'
   	then tt'
	else packT1 tt'

-- | Pack a kind into standard form.
packK :: Kind -> Kind
packK kk
 = let kk'	= packK1 kk
   in  if kk == kk'
   	then kk'
	else packK1 kk'


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
	TForall v1 k1 (TFree v2 t2)
	 -> let t2'	= packT1 t2
	    in	TFree v2 (TForall v1 k1 t2')

 	TForall v k1 t2
	 -> let	t2'	= packT1 t2
	    in	TForall v k1 t2'

	 
	TContext k1 t2
	 -> let	k1'	= packK1 k1
	 	t2'	= packT1 t2
	    in 	TContext k1' t2'

	TFetters (TVar k v1) [FWhere (TVar _ v2) t2]
	 | v1 == v2
	 -> t2
	 
	TFetters t1 fs
	 -> let t1'	= packT1 t1
	 	fs'	= restrictBinds t1' fs
	    in	makeTFetters t1' fs'


	-- Crush witnesses along the way
	TApp t1@(TCon (TyConClass tyClass k)) t2
	 -> let result
			-- crush LazyH on the way
	 		| tyClass == TyClassLazyH
			, Just (vD, k, (TVar KRegion r : ts))	<- takeTData t2
			= TApp (TCon tcLazy) (TVar KRegion r)

			-- crush MutableT on the way
			| tyClass == TyClassMutableT 
			, Just _		<- takeTData t2
			, (rs, ds)		<- slurpVarsRD t2
			= TWitJoin 
			    	$ (   map (\r -> TApp (TCon tcMutable)  r) rs
				   ++ map (\d -> TApp (TCon tcMutableT) d) ds)

			| otherwise
			= let	t2'	= packT1 t2
			  in	TApp t1 t2'
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
	 	
	-- mask of bottom is just bottom
	TMask k (TBot k1) t2
	 	| k == k1
		-> TBot k1
	 
	-- combine mask of mask
	TMask k1 (TMask k2 t1 t2) t3
	 | k1 == k2
	 -> makeTMask k1 t1 (makeTSum k1 [t2, t3])
	 
	-- in core, all closure vars are quantified, and fully sunk
	--	so if we can't see some tagged component, it's not there.
	TMask k1 t1@(TVar k2 v) t3
	 | k1 == k2
	 , k1 == KClosure
	 -> t1
	 
	TMask k t1 t2
	 -> let t1'	= packT1 t1
	 	t2'	= packT1 t2
	    in	applyTMask $ TMask k t1' t2'
	    
	TVar k v	-> tt
	TVarMore k v t	-> TVarMore k v (packT1 t)

	TCon{}		-> tt

	TBot k		-> tt
	TTop k 		-> tt
	 
	-- effect
	-- crush compound effects along the way
	TEffect v [t1]
	 -> let result
			-- ReadH 
	 		| v == primReadH
			= case takeTData t1 of
				Just (vD, k, (TVar KRegion r : ts)) 
					-> TEffect primRead [TVar KRegion r]
				
				Just (vD, k, [])
					-> TBot KEffect

				Nothing	-> tt
					
			-- ReadT
			| v == primReadT
			= case takeTData t1 of
				Just (vD, k, ts)
				 -> let (tRs, tDs) = unzip $ map slurpVarsRD ts
				    in  makeTSum KEffect
						(  [TEffect primRead  [t]	| t <- concat tRs]
						++ [TEffect primReadT [t]	| t <- concat tDs] )

				Nothing	-> tt
				
			-- WriteT
			| v == primWriteT
			= case takeTData t1 of
				Just (vD, k, ts)
				 -> let (tRs, tDs) = unzip $ map slurpVarsRD ts
				    in  makeTSum KEffect
						(  [TEffect primWrite  [t]	| t <- concat tRs]
						++ [TEffect primWriteT [t]	| t <- concat tDs] )

				Nothing	-> tt
	
			-- some other effect
			| otherwise
			= TEffect v [packT1 t1]

	    in	result
	    
	TEffect v ts
	 -> TEffect v (map packT1 ts)
	    
	-- closure
	TFree v (TBot KClosure)
	 -> TBot KClosure

	TFree v1 (TFree v2 t2)
	 -> TFree v1 t2

	TFree v1 (TSum KClosure ts)
	 -> TSum KClosure (map (TFree v1) ts)

	TFree v t1
	 -> let t1'	= packT1 t1
	    in	TFree v t1'
	    
	TTag v	-> tt

	TWitJoin ts
	 -> makeTWitnessJoin (map packT1 ts)
	
	-- wildcards
	TWild{}	-> tt

	_ -> panic stage
		$ "packT: no match for " % tt % "\n"
		% "  tt = " % show tt % "\n"


slurpTFetters (TFetters t fs)	= (t, fs)
slurpTFetters tt		= (tt, [])


-- | Do one round of packing on this kind
packK1 :: Kind -> Kind
packK1 kk
	-- crush LazyH on the way
	| KClass tc [t1]	<- kk
	, Just (vD, k, TVar KRegion r : ts)	<- takeTData t1
	, tc == TyClassLazyH
	= KClass TyClassLazy [TVar KRegion r]

	-- crush MutableT on the way
	| KClass tc [t1]	<- kk
	, Just _			<- takeTData t1
	, tc == TyClassMutableT
	, (rs, ds)			<- slurpVarsRD t1
	= makeKWitJoin 
	    	$ (   map (\r -> KClass TyClassMutable  [r]) rs
		   ++ map (\d -> KClass TyClassMutableT [d]) ds)
	
	| KClass v ts	<- kk
	= KClass v (map packT1 ts)
	    
	| otherwise
	= kk



-----
-- inlineTWheresT
--	Inline all TLet expressions in this type.
--	
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
	 		= partitionFs [(=@=) FWhere{}, (=@=) FMore{}] fs
		
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
	TMask k t1 t2		-> TMask k 	(down t1) (down t2)
	    
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
	TTag v			-> tt

	TWild k			-> tt
	    


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

