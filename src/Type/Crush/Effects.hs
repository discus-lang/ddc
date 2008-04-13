{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crush effects into their parts.

module Type.Crush.Effects
	( crushEffectC )
where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim
import Shared.Var		(VarBind, NameSpace(..))
import Shared.Error

import Type.Trace
import Type.Exp
import Type.Util
import Type.State
import Type.Class
import Type.Feed
import Type.Pretty
import Type.Location
import Type.Util.Pack

import Type.Check.GraphicalData
import Type.Crush.Unify

import Type.Plate.Collect	(collectClassIds)
import Type.Plate.Trans

-----
debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Crush.Effect"

-- Try and crush the effect in this node.
crushEffectC 
	:: ClassId 
	-> SquidM Bool	-- whether we crushed something from this class

crushEffectC cid
 = do	Just c		<- lookupClass cid
	trace	$ "*   crushEffectC " 	% cid			% "\n"
		% "    eff    = " % classType c			% "\n"

	crushEffectC2 cid c

crushEffectC2 cid (ClassForward cid')
	= crushEffectC cid'

crushEffectC2 cid (Class { classType = Just eff })
 = do
	let effs		=  flattenTSum eff

	(effs_crushed, newNodes)
		<- crushMilkEffects cid effs [] []
		
	if effs == effs_crushed
	 then do	
	 	trace	$ ppr "\n"
		
		-- if the class still contains effect constructors then it needs to remain
		--	active so crushEffects gets called on the next grind
		when (isCrushable eff)
		 $ activateClass cid

		return False

	 else do
	 	let eff'	= makeTSum KEffect effs_crushed
		trace	$ "    eff' = " % eff % "\n\n"
			
		modifyClass cid
		 $ \c -> c 	{ classType  = Just eff' 
		 		, classNodes = newNodes ++ (classNodes c) }
		 
		activateClass cid
	
		return True

crushEffectC2 cid _
	= return False
		

-- Try and crush a list of effects, keeping effects that don't crush,
--	and remembering nodes of the extra crushed parts.
crushMilkEffects
	:: ClassId
	-> [Effect] 
	-> [Effect] -> [(Effect, TypeSource)]
	-> SquidM ([Effect], [(Effect, TypeSource)])
	
crushMilkEffects cid [] effAcc nodeAcc
	= return (reverse effAcc, reverse nodeAcc)
	
crushMilkEffects cid (e : es) effAcc nodeAcc
 = do	mCrushed	<- crushEffectT cid e
 
 	case mCrushed of
	 Nothing		
	 	-> crushMilkEffects cid es (e : effAcc) nodeAcc

	 Just n@(eCrushed, tsCrushed)
		-> crushMilkEffects cid es (eCrushed : effAcc) (n : nodeAcc)
		
		
-- Try and crush this effect into parts.
crushEffectT 
	:: ClassId
	-> Effect -> SquidM (Maybe (Effect, TypeSource))

crushEffectT cid tt@(TEffect ve [TClass k cidT])
 = do	
	-- the effect in the original class operates on a classId, so we need to 
	--	look up this type before we can crush the effect.
 	Just (Class { classType = mType })
			<- lookupClass cidT

	case mType of
	 Just tNode	-> return $ crushEffectT' cid tt tNode
	 _		-> return Nothing
	
crushEffectT cid tt
	= return Nothing

crushEffectT' cid tt tNode

	-- Read of outer constructor of object.
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EReadH
	= case tNode of
		TData _ v (tR : ts)	
			-> Just ( TEffect primRead [tR]
				, TSI $ SICrushedE cid tt)

		TData _ v []		
			-> Just ( TBot KEffect
				, TSI $ SICrushedE cid tt)

		_	-> Nothing
	


	-- Read of whole object. (deep read).
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EReadT
	, TData{}		<- tNode
	= let
		(rs, ds)	= slurpVarsRD tNode
		esRegion	= map (\r -> TEffect primRead  [r]) rs
		esType		= map (\d -> TEffect primReadT [d]) ds

	  in 	Just 	( makeTSum KEffect $ (esRegion ++ esType)
		  	, TSI $ SICrushedE cid tt)


	-- Write of whole object. (deep write)
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EWriteT
	, TData{}		<- tNode
	= let	
		(rs, ds)	= slurpVarsRD tNode
		esRegion	= map (\r -> TEffect primWrite  [r])   rs
		esType	= map (\d -> TEffect primWriteT [d]) ds
				
	  in	Just	( makeTSum KEffect $ (esRegion ++ esType)
	  		, TSI $ SICrushedE cid tt)

	-- can't crush this one
	| otherwise
	= Nothing


	
-- | Checks whether this effect might ever need to be crushed
isCrushable :: Effect -> Bool
isCrushable eff
 = case eff of
 	TEffect v _
	 | elem v [primReadH, primReadT, primWriteT]	-> True
	 | otherwise					-> False
	 
	TSum KEffect ts
	 -> or $ map isCrushable ts
	 
	TClass{}
	 -> False
	
	TVar{}	-> False
	TBot{}	-> False

	_ ->  panic stage $ "isCrushable: no match for " % eff % "\n"
