{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crush effects into their parts.
module Type.Crush.Effects
	(crushEffectInClass)
where
import Type.Exp
import Type.Feed
import Type.Builtin
import Type.Util
import Type.State
import Type.Class
import Type.Location
import Util
import DDC.Main.Error

debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Crush.Effects"


-- Try and crush the effect in this node into a simpler form.
--
-- @
--              TApp
--            /       \                            TApp
--   TCon DeepRead       TApp           =>       /      \
--                     /      \             TCon Read    TVar r1
--                  TCon Int   TVar r1
-- @
--
crushEffectInClass 
	:: ClassId			-- ^ Id for the class containing the top level application.
	-> SquidM Bool			-- ^ whether we crushed something from this class.

crushEffectInClass cid
 = do	
	-- Grab the class from the graph
	Just cls	<- lookupClass cid

	trace	$ vcat
		[ "--  crushEffectInClass " 	% cid
		, "    type      = " 		% classType cls
		, "    typeSources\n"		%> punc "\n" (classTypeSources cls) 
		, newline]

	crushEffectWithClass2 cid cls

-- This class points somewhere else.
crushEffectWithClass2 cid 
	cls@(ClassForward cid')
	= crushEffectInClass cid'

-- A class containing an application of some effect.
crushEffectWithClass2 cid 
	cls@(Class { classType = Just tApp@(TApp t1@(TClass k1 cid1) t2@(TClass k2 cid2)) })
 = do
	-- Get the source of the effect.
	tAppSrc		<- getSourceOfEffect tApp cls

	-- Get the function and argument types of the application from the graph.
	Just cls1	<- lookupClass cid1
	Just cls2	<- lookupClass cid2

	trace	$ vcat
		[ "    source          = "	% tAppSrc
		, "    cls1.classType  = " 	% classType cls1
		, "    cls2.classType  = "	% classType cls2 
		, blank ]
	
	let result
		-- We've got enough information to try and crush the effect.
		| Just tEff	<- classType cls1
		, Just tArg	<- classType cls2
		= do	mEffSrc	<- crushEffect cid cls tApp tAppSrc tEff tArg cid2
			crushEffectWithClass_update cid cls mEffSrc
		
		-- If the argument is Nothing then we're waiting it for to be handled by the unifier.
		--	Set ourselves active so we get called again on the next crushing pass.
		| Just tEff	<- classType cls1
		, elem tEff [tHeadRead, tDeepRead, tDeepWrite]
		= case classType cls2 of
			Nothing	
			 -> do	activateClass cid
				return False
				
			Just (TSum _ [])
			 -> do	activateClass cid
				return False
						
		-- Can't crush the effect. Maybe its arg hasn't been unified yet..
		| otherwise
		=	return False
			
	result
	
crushEffectWithClass2 _ _
	= return False
	

-- We've crushed the effect. Now update the class.
crushEffectWithClass_update cid cls Nothing
 = 	return False

	
crushEffectWithClass_update cid cls (Just (eff', src'))
 = do	trace	$ vcat
		[ "    effCrushed      = "	% eff'
		, "    srcCrushed      = "	% src'
		, newline ]

	-- Add the new effect to the graph.
	Just tt'	<- feedTypeWithSource src' Nothing eff'

	-- Update the original class to point to our new effect.
	modifyClass cid
	 $ \c -> c 	{ classType  	   = Just eff' 
		 	, classTypeSources = (eff', src') : classTypeSources c }

	-- If the crushed effect still contains crushable components then activate ourselves
	--	again so we get called on the next pass
	when (isCrushable eff') $ activateClass cid

	return True


-- | Crush an single effect.
crushEffect
	:: ClassId		-- ^ Id for the class containing the top level application.
	-> Class		-- ^ Class containing the top level application. (tEff tArg)
	-> Type			-- ^ The top level application (tEff tArg)
	-> TypeSource		-- ^ The source of the top level application
	-> Type			-- ^ Type being applied. (tEff)
	-> Type			-- ^ Type being applied (tArg)
	-> ClassId		-- ^ Id of class containing type being applied (cidArg)
	-> SquidM (Maybe (Effect, TypeSource))
	
crushEffect cid cls tApp tAppSrc tEff tArg cidArg

	-- Effects on single constructors.
	--	When we do a case match on a unit value we get an effect HeadRead (),
	--	but () doesn't have a head region, so we can just drop the effect.
	| elem tEff [tHeadRead, tDeepRead, tDeepWrite]
	, TCon{}	<- tArg
	= return $ Just
		( tPure
		, TSI $ SICrushedES cid tApp tAppSrc )


	-- HeadRead
	| tEff		== tHeadRead
	, TApp{}	<- tArg
	= do	
		-- Start from the class holding the argument, 
		--	and walk down its left spine to get its head region, if any.
		mHead	<- headTypeDownLeftSpine cidArg
		
		case mHead of
			Just tR	-> return $ Just
					( TApp tRead tR
					, TSI $ SICrushedES cid tApp tAppSrc )
			
			Nothing	-> return Nothing
			
		
	-- DeepRead
	| tEff		== tDeepRead
	, TApp{}	<- tArg
	= do	
		-- Start from the class holding the argument type,
		--	and walk down the spine gathering a list of argments.
		mtsArgs	<- traceDownLeftSpine tArg
		
		case mtsArgs of
		 Nothing	
		  -> do activateClass cid
			return Nothing

		 Just (con:tsArgs)
		  -> do	let  mesBits	
				= map (\t -> case t of
						TCon{}		-> Nothing
						TSum _ []	-> Nothing
						TClass k _
						 | k == kRegion	 -> Just (TApp tRead t)
						 | k == kEffect  -> Nothing
						 | k == kClosure -> Nothing
						 | k == kValue   -> Just (TApp tDeepRead t)
						 | otherwise	 -> Just (TApp tDeepRead t))	
				$ tsArgs
		
			return	$ Just
				( makeTSum kEffect (catMaybes mesBits)
				, TSI $ SICrushedES cid tApp tAppSrc)


	-- DeepWrite
	| tEff		== tDeepWrite
	, TApp{}	<- tArg
	= do	
		-- Start from the class holding the argument type,
		--	and walk down the spine gathering a list of argments.
		mtsArgs	<- traceDownLeftSpine tArg

		case mtsArgs of
		 Nothing	
		  -> do	activateClass cid
			return Nothing
			
		 Just (con:tsArgs)
		  -> do	let  mesBits	
				= map (\t -> case t of
						TCon{}		-> Nothing
						TSum _ []	-> Nothing
						TClass k _
						 | k == kRegion	 -> Just (TApp tWrite t)
						 | k == kEffect  -> Nothing
						 | k == kClosure -> Nothing
						 | k == kValue   -> Just (TApp tDeepWrite t)
						 | otherwise	 -> Just (TApp tDeepWrite t))
				$ tsArgs

			return	$ Just
				( makeTSum kEffect (catMaybes mesBits)
				, TSI $ SICrushedES cid tApp tAppSrc)
	
	
	| otherwise
	= do	when (isCrushable (TApp tEff tArg) )
		 $ do	trace $ ppr "reactivating\n"
			activateClass cid

		return Nothing
	

-- | Get the source of some effect, given the class that contains it.
--   This function applies the current subsitution to both the provided effect,
--   and the types in the class, so that we can properly test them for equality.
getSourceOfEffect 
	:: Effect
	-> Class 
	-> SquidM TypeSource

getSourceOfEffect eff_ cls
 = do	
	let updateFstVC (x, src)
	     = do 	x'	<- updateVC x
			return (x', src)
			
	eff	<- updateVC eff_
	ts_src	<- mapM updateFstVC $ classTypeSources cls

	-- lookup the type-source for this effect
	--	The nodes hold effect sums, so we need to look inside them
	--	to find which one holds our (atomic) conflicting effect
	let src : _ 	
	     = [nodeSrc	
			| (nodeEff,  nodeSrc)	<- ts_src
			, elem eff $ flattenTSum nodeEff]

	return src
	
	
-- | Checks whether this effect might ever need to be crushed
isCrushable :: Effect -> Bool
isCrushable eff
 = case eff of
	TApp tE _
	 -> elem tE [tHeadRead, tDeepRead, tDeepWrite]
	 
	TSum kE ts
	 | kE == kEffect 
	 -> or $ map isCrushable ts
	 
	TClass{}	-> False
	TVar{}		-> False
	TCon{}		-> False
	_ 		->  panic stage $ "isCrushable: no match for " % eff % "\n"


