{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crush effects into their parts.
module Type.Crush.Effects
	(crushEffectC)
where
import Type.Exp
import Type.Util
import Type.State
import Type.Class
import Type.Location
import Shared.VarPrim
import Util
import DDC.Main.Error
import qualified DDC.Var.PrimId	as Var
import qualified Shared.Var	as Var

-----
debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Crush.Effect"


-- Try and crush the effect in this node.
crushEffectC 
	:: ClassId	-- classid for the node containg the effect we want to crush.
	-> SquidM Bool	-- whether we crushed something from this class

crushEffectC cid
 = do	Just cls	<- lookupClass cid
	trace	$ "*   crushEffectC " 		% cid			% "\n"
		% "    eff           = " 	% classType cls		% "\n"
		% "    typeSources\n"
		% "\n" %!% classTypeSources cls	% "\n"

	crushEffectC2 cid cls

-- this class points somewhere else.
crushEffectC2 cid (ClassForward cid')
	= crushEffectC cid'

crushEffectC2 cid cls@(Class { classType = Just eff })
 = do
	-- flatten the sum into its atomic effects.
	let effs		=  flattenTSum eff

	-- keep crushing until no more effects come out.
	(effs_crushed, newNodes)
		<- crushMilkEffects cid cls effs [] []
		
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
	-> Class
	-> [Effect] 
	-> [Effect] 
	-> [(Effect, TypeSource)]
	-> SquidM ([Effect], [(Effect, TypeSource)])
	
crushMilkEffects cid cls [] effAcc nodeAcc
	= return (reverse effAcc, reverse nodeAcc)
	
crushMilkEffects cid cls (e : es) effAcc nodeAcc
 = do	mCrushed	<- crushEffectC_parts cid cls e
 
 	case mCrushed of
	 Nothing		
	 	-> crushMilkEffects cid cls es (e : effAcc) nodeAcc

	 Just n@(eCrushed, tsCrushed)
		-> crushMilkEffects cid cls es (eCrushed : effAcc) (n : nodeAcc)
		
		
-- Try and crush this effect into parts.
crushEffectC_parts
	:: ClassId
	-> Class
	-> Effect 
	-> SquidM (Maybe (Effect, TypeSource))

crushEffectC_parts cid cls eff_@(TEffect ve [TClass k cidT])
 = do	
	-- the effect in the original class operates on a classId, so we need to 
	--	look up this type before we can crush the effect.
 	Just (Class { classType = mType })
			<- lookupClass cidT

	let updateFstVC (x, src)
	     = do 	x'	<- updateVC x
			return (x', src)
			
	eff	<- updateVC eff_
	ts_src	<- mapM updateFstVC $ classTypeSources cls

	
	trace	$ "    eff_atomic    = " % eff 		% "\n"
		% "    eff_target    = " % mType	% "\n"
		% "    ts_src:\n"
		%> "\n" %!% ts_src

	-- lookup the type-source for this effect
	--	The nodes hold effect sums, so we need to look inside them
	--	to find which one holds our (atomic) conflicting effect

	let effSrc : _ 	
	     = [nodeEffSrc	
			| (nodeEff,  nodeEffSrc)	<- ts_src
			, elem eff $ flattenTSum nodeEff]
 
	case mType of
	 Just tNode	
	  -> do	mParts	<- crushEffectT_node cid eff effSrc tNode
	  	trace $ "    eff_atomic_crushed = " % mParts % "\n\n"
		return	mParts

	 _ -> 	return 	Nothing

crushEffectC_parts cid cls tt
	= return Nothing


-- | Try and crush an effect into smaller parts.
crushEffectT_node 
	:: ClassId 			-- classId of the node containing the effect 
	-> Effect 			-- the effect to be crushed
	-> TypeSource			-- the source of the effect
	-> Type 			-- the target type that the effect is operating on.
	-> SquidM 
		(Maybe  ( Effect	-- If  Just (eff, src)  we want to replace the original effect 
			, TypeSource))	-- with this one, else keep the original.

crushEffectT_node cid eff effSrc tNode

	-- ReadH (Read of head region) where target is a type application.
	| TEffect vE [TClass k cidT]	<- eff
	, TApp{}			<- tNode
	, Var.varId vE == Var.VarIdPrim Var.EReadH
	= do	
		-- examine the type being constrained to see if we know its head region yet.
		mHead	<- headTypeDownLeftSpine cidT

		case mHead of
		 	Just t  -> return $ Just
					( TEffect primRead [t]
					, TSI $ SICrushedES cid eff effSrc )
			Nothing -> return Nothing
			
	-- ReadH (Read of head region) where target is a single constructor.
	-- 	For an effect like  ReadH ()  we just discard the whole thing.
	--	Such an effect can arise if we do a case match on an value of unit type.
	| TEffect vE [TClass k cidT] 	<- eff
	, TCon{}			<- tNode
	, Var.varId vE == Var.VarIdPrim Var.EReadH
	=	return	$ Just 
			( tPure
			, TSI $ SICrushedES cid eff effSrc)

	-- ReadT (deep read of whole object)
	| TEffect ve [t1]	<- eff
	, Var.varId ve == Var.VarIdPrim Var.EReadT
	= do	mtsArgs	<- traceDownLeftSpine t1
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
						 | otherwise	 -> Just (TEffect primReadT [t]))	
				$ tsArgs
		
			return	$ Just
				( makeTSum kEffect (catMaybes mesBits)
				, TSI $ SICrushedES cid eff effSrc)
						
	-- WriteT (deep write to whole object)
	| TEffect ve [t1]	<- eff
	, Var.varId ve == Var.VarIdPrim Var.EWriteT
	= do	mtsArgs	<- traceDownLeftSpine t1
		case mtsArgs of
		 Nothing	-> return Nothing
		 Just tsArgs
		  -> do	let  mesBits	
				= map (\t -> case t of
						TCon{}		-> Nothing
						TBot{}		-> Nothing
						TClass k _
						 | k == kRegion	 -> Just (TEffect primWrite  [t])
						 | k == kEffect  -> Nothing
						 | k == kClosure -> Nothing
						 | k == kValue   -> Just (TEffect primWriteT [t])
						 | otherwise	 -> Just (TEffect primWriteT [t]))	
				$ tsArgs

			return	$ Just
				( makeTSum kEffect (catMaybes mesBits)
				, TSI $ SICrushedES cid eff effSrc)
	
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


