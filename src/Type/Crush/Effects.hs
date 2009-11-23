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
	:: ClassId	-- classid for the node containg the effect we want to crush.
	-> SquidM Bool	-- whether we crushed something from this class

crushEffectC cid
 = do	Just c		<- lookupClass cid
	trace	$ "*   crushEffectC " 	% cid			% "\n"
		% "    eff    = " % classType c			% "\n"

	crushEffectC2 cid c

-- this class points somewhere else.
crushEffectC2 cid (ClassForward cid')
	= crushEffectC cid'

crushEffectC2 cid (Class { classType = Just eff })
 = do
	-- flatten the sum into its atomic effects.
	let effs		=  flattenTSum eff

	-- keep crushing until no more effects come out.
	(effs_crushed, newNodes)
		<- crushMilkEffects cid effs [] []
		
	if effs == effs_crushed
	 then do	
	 	trace	$ ppr "\n"
		
		-- if the class still contains effect constructors then it needs
		--	to remain active so crushEffects gets called on the next grind
		when (isCrushable eff)
		 $ activateClass cid

		return False

	 else do
	 	let eff'	= makeTSum kEffect effs_crushed
		trace	$ "    eff'     = " % eff % "\n\n"
			
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
	-> [Effect] 
	-> [(Effect, TypeSource)]
	-> SquidM ([Effect], [(Effect, TypeSource)])
	
crushMilkEffects cid [] effAcc nodeAcc
	= return (reverse effAcc, reverse nodeAcc)
	
crushMilkEffects cid (e : es) effAcc nodeAcc
 = do	mCrushed	<- crushEffectC_parts cid e
 
 	case mCrushed of
	 Nothing		
	 	-> crushMilkEffects cid es (e : effAcc) nodeAcc

	 Just n@(eCrushed, tsCrushed)
		-> crushMilkEffects cid es (eCrushed : effAcc) (n : nodeAcc)
		
		
-- Try and crush this effect into parts.
crushEffectC_parts
	:: ClassId
	-> Effect -> SquidM (Maybe (Effect, TypeSource))

crushEffectC_parts cid tt@(TEffect ve [TClass k cidT])
 = do	
	-- the effect in the original class operates on a classId, so we need to 
	--	look up this type before we can crush the effect.
 	Just (Class { classType = mType })
			<- lookupClass cidT
	
	trace	$ "    atomic eff    = " % tt 	% "\n"
		% "    type acted on = " % mType	% "\n"

	case mType of
	 Just tNode	
	  -> do	let mParts	= crushEffectT_node cid tt tNode
	  	trace $ "    crushed parts = " % mParts % "\n\n"
		return	mParts

	 _ -> 	return 	Nothing

crushEffectC_parts cid tt
	= return Nothing


crushEffectT_node 
	:: ClassId 
	-> Type 
	-> Type 
	-> Maybe (Effect, TypeSource)

crushEffectT_node cid tt tNode

	-- Read of outer constructor of object.
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EReadH
	= let result
		| Just	(v, k, tR : ts)	<- takeTData tNode
		= Just	( TEffect primRead [tR]
			, TSI $ SICrushedE cid tt)
			
		| Just	(v, k, [])	<- takeTData tNode
		= Just	( tPure
			, TSI $ SICrushedE cid tt)

		| otherwise
		= Nothing
	  in	result

	-- Read of whole object. (deep read).
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EReadT
	= case tNode of
		TCon{}
		 -> Just ( tPure
			 , TSI $ SICrushedE cid tt)
	
		TBot k
		 | k == kRegion
		 -> Just ( TEffect primRead [t1]
			 , TSI $ SICrushedE cid tt)
	
		TApp t1 t2	
		 -> Just ( makeTSum kEffect 
				[ TEffect primReadT [t1]
				, TEffect primReadT [t2]] 
		    	 , TSI $ SICrushedE cid tt)
		
		_ -> Nothing
				
	-- Write of whole object. (deep write)
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EWriteT
	= case tNode of
		TBot k
		 | k == kRegion
		 -> Just ( TEffect primWrite [t1]
			 , TSI $ SICrushedE cid tt)
		
		TCon{}
		 -> Just ( tPure
			 , TSI $ SICrushedE cid tt)
			
			
		TApp t1 t2
		 -> Just ( makeTSum kEffect
				[ TEffect primWriteT [t1]
				, TEffect primWriteT [t2]]
			 , TSI $ SICrushedE cid tt)

		_ -> Nothing
	
	-- can't crush this one
	| otherwise
	= Nothing

	
-- | Checks whether this effect might ever need to be crushed
isCrushable :: Effect -> Bool
isCrushable eff
 = case eff of
 	TEffect v _	-> elem v [primReadH, primReadT, primWriteT]
	 
	TSum kE ts
	 | kE == kEffect 
	 -> or $ map isCrushable ts
	 
	TClass{}	-> False
	TVar{}		-> False
	TBot{}		-> False
	_ 		->  panic stage $ "isCrushable: no match for " % eff % "\n"


