{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crush effects into their parts.
module Type.Crush.Effects
	( crushEffectC )
where

import Type.Trace
import Type.Exp
import Type.Util
import Type.State
import Type.Class
import Type.Feed
import Type.Pretty
import Type.Location
import Type.Check.GraphicalData
import Type.Crush.Unify
import Type.Plate.Collect	(collectClassIds)
import Type.Plate.Trans
import Shared.VarPrim
import Shared.Var		(VarBind, NameSpace(..))
import Shared.Error
import Util
import Data.Map			(Map)
import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var

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
	trace	$ "*   crushEffectC " 		% cid			% "\n"
		% "    eff           = " 	% classType c		% "\n"

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
		trace	$ "    eff'     = " % eff' % "\n\n"
			
		modifyClass cid
		 $ \c -> c 	{ classType  	  = Just eff' 
		 		, classTypeSources = newNodes ++ (classTypeSources c) }
		 
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
	
	trace	$ "    eff_atomic    = " % tt 		% "\n"
		% "    eff_target    = " % mType	% "\n"

	case mType of
	 Just tNode	
	  -> do	mParts	<- crushEffectT_node cid tt tNode
	  	trace $ "    eff_atomic_crushed = " % mParts % "\n\n"
		return	mParts

	 _ -> 	return 	Nothing

crushEffectC_parts cid tt
	= return Nothing


-- | Try and crush an effect into smaller parts.
crushEffectT_node 
	:: ClassId 			-- classId of the node containing the effect 
	-> Effect 			-- the effect to be crushed
	-> Type 			-- the target type that the effect is operating on.
	-> SquidM 
		(Maybe  ( Effect	-- If  Just (eff, src)  we want to replace the original effect 
			, TypeSource))	-- with this one, else keep the original.

crushEffectT_node cid tt tNode

	-- ReadH (Read of head region) where target is a type application.
	| TEffect vE [TClass k cidTarget]	<- tt
	, TApp{}		<- tNode
	, Var.bind vE == Var.EReadH
	= do	
		-- examine the type being constrained to see if we know its head region yet.
		mHead	<- headTypeDownLeftSpine cidTarget

		case mHead of
		 	Just t  -> return $ Just
					( TEffect primRead [t]
					, TSI $ SICrushedE cid tt )
			Nothing -> return Nothing
			

	-- ReadH (Read of head region) where target is a single constructor.
	-- 	For an effect like  ReadH ()  we just discard the whole thing.
	--	Such an effect can arise if we do a case match on an value of unit type.
	| TEffect vE [TClass k cidT] <- tt
	, TCon{}		<- tNode
	, Var.bind vE == Var.EReadH
	=	return	$ Just 
			( tPure
			, TSI $ SICrushedE cid tt)


	-- ReadT (deep read of whole object)
	| TEffect ve [t1@(TClass _ cidT)]	<- tt
	, Var.bind ve == Var.EReadT
	= do	mtsArgs	<- traceDownLeftSpine t1
		trace	$ "    tsArgs = " % mtsArgs 	% "\n\n"
		
		case mtsArgs of
		 Nothing	-> return Nothing
		 Just tsArgs
		  -> do	let  mesBits	
				= map (\t -> case t of
						TCon{}		-> Nothing
						TBot{}		-> Nothing
						TClass k _
						 | k == kRegion	 -> Just (TEffect primRead  [t])
						 | k == kEffect  -> Nothing
						 | k == kClosure -> Nothing
						 | k == kValue   -> Just (TEffect primReadT [t])
						
						 | otherwise	 
--						 -> freakout stage 
--							("crushEffectT_node: dodgy ReadT " % t % " of kind " % k % "\n"
--							% "  mtsArgs = " % mtsArgs)
						 -> Just (TEffect primReadT [t])
					)	
				$ tsArgs
		
			return	$ Just
				( makeTSum kEffect (catMaybes mesBits)
				, TSI $ SICrushedE cid tt)
						
{-
	return 
	$ case tNode of
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
-}				
	-- WriteT (deep write to whole object)
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EWriteT
	= return 
	$ case tNode of
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
	= return Nothing

	
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


