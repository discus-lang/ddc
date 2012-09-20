{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-record-updates #-}

module DDC.Solve.Crush.Effects
	(crushEffectsInClass)
where
import DDC.Type
import DDC.Solve.Graph
import DDC.Solve.State
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Solve.Walk
import DDC.Solve.Location
import DDC.Solve.Feed
import Control.Monad
import Data.Maybe

debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Crush.Effects"

-- CrushResult ------------------------------------------------------------------------------------
-- | The result of crushing a single node effect in a class.
data CrushResult
	-- | This effect can never be crushed because it's already atomic 
	--   (like Read %r1)
	= CrushNever
	
	-- | We might be able to crush this effect later if 
	--   a more specific type is unified into its argument. 
	--   (like ReadT a)
	| CrushMaybeLater

	-- | We've completely crushed out this effect, and can replace it by bottom.
	--   (like ReadH ())
	| CrushBottom
	
	-- | We've simplified this effect into some smaller ones.
	| CrushSimplified [(Node, TypeSource)]
	deriving Show

instance Pretty CrushResult PMode where
 ppr result
  = case result of
	CrushNever		-> ppr "CrushNever"
	CrushMaybeLater		-> ppr "CrushMaybeLater"
	CrushBottom		-> ppr "CrushBottom"
	CrushSimplified	bits	-> ppr $ "CrushSimplified " % bits


-- | Given a sourced node and the result of crushing it, 
--   return what we should replace that node by in the graph class.
takeNewNodeSrc 
	:: ((Node, TypeSource), CrushResult)
	-> [(Node, TypeSource)]

takeNewNodeSrc (orig, crush)
 = case crush of
	CrushNever		-> [orig]
	CrushMaybeLater		-> [orig]
	CrushBottom		-> []
	CrushSimplified bits	-> bits


-- | Check if a CrushResult will change the corresponding node in the class.
isChangeResult :: CrushResult -> Bool
isChangeResult crush
 = case crush of
	CrushNever		-> False
	CrushMaybeLater		-> False
	CrushBottom		-> True
	CrushSimplified{}	-> True
	

-- | Check if a CrushResult says we should activate the class and try crushing later.
isActiveResult :: CrushResult -> Bool
isActiveResult crush
 = case crush of
	CrushNever		-> False
	CrushMaybeLater		-> True
	CrushBottom		-> False
	CrushSimplified{}	-> True

	

-- Crushing ---------------------------------------------------------------------------------------
-- Try and crush the effects in this clas into a simpler form.
--
-- @
--              TApp
--            /       \                            TApp
--   TCon DeepRead       TApp           =>       /      \
--                     /      \             TCon Read    TVar r1
--                  TCon Int   TVar r1
-- @
--
crushEffectsInClass 
	:: ClassId		-- ^ cid of the class containing the top level application.
	-> SquidM Bool		-- ^ Whether we made progress by crushing something in this class.

crushEffectsInClass cid
 = do	Just cls	<- lookupClass cid
	crushEffectsWithClass cid cls
	
crushEffectsWithClass cid cls
 = case cls of

	-- This isn't an effect class.
	-- The grinder shouldn't be calling us here.
	ClassUnallocated
	 -> panic stage $ "crushEffectWithClass: ClassUnallocated " % cid

	ClassFetterDeleted{}
	 -> panic stage $ "crushEffectWithClass: ClassFetterDeleted " % cid 

	ClassFetter{}
	 -> panic stage $ "crushEffectWithClass: ClassFetter " % cid
	
	-- Follow indirections.
	ClassForward _ cid'
	 -> crushEffectsInClass cid'
		
	-- A class containing an application.
	Class	{ classKind = kind }
	 | isEffectKind kind
	 -> do	trace	$ vcat 
			[ "-- Crush.effect " 	% cid 
			, "   nodes   = " 	% map fst (classTypeSources cls) ]
	
		-- Try to crush each effect in turn.
		results	<- mapM (crushEffectNodeSrcOfClass cid) 
			$ classTypeSources cls
			
		-- Compare the result of crushing each node with the original, 
		-- to determine what we need to replace them by.
		let nodeResults		= zip (classTypeSources cls) results
		let nodeReplacements	= concatMap takeNewNodeSrc nodeResults
			
		trace	$ vcat
			[ ppr "   results:" 
		 	, vcat 	$ map (\((n, s), crush) -> "      " % padL 20 n % s % "\n" 
							%  "      " % crush) 
				$ nodeResults 

			, ppr "   replacements:"
			, vcat 	$ map (\(n, s) 		-> "      " % padL 20 n % s) 
				$ nodeReplacements ]
		
		-- Update the class with the replacement nodes if needed.
		let classHasChanged 
			= or $ map isChangeResult results

		-- If we're changing the class we must reactivate it as we might be adding
		-- another compound effect like ReadT that needs further crushing.
		when classHasChanged
		 $ updateClass True cid cls { classTypeSources = nodeReplacements }
		
		-- Even if nothing has changed, we might be able to crush
		-- something next time.
		when ((not classHasChanged) && (or $ map isActiveResult results))
		 $ activateClass cid		
		
		return classHasChanged
		
	 -- some non-effect class.
	 | otherwise
	 -> return False


-- | Try to crush a node in an effect class.
crushEffectNodeSrcOfClass 
	:: ClassId 
	-> (Node, TypeSource)
	-> SquidM CrushResult

crushEffectNodeSrcOfClass cid (node, src)

	-- All the nodes we can crush are applications.
	| NApp{} <- node
	= do	nApp@(NApp cidCon cidArg) <- sinkCidsInNode node
		Just clsCon	<- lookupClass cidCon
		Just clsArg	<- lookupClass cidArg
		
		-- We have to wait until both parts of the application
		-- have been unified so we have something to work with.
		trace	$ vcat
			[ "    node                = "	% nApp 
			, "    clsCon.classUnified = "	% classUnified clsCon
			, "    clsArg.classUnified = "	% classUnified clsArg ]

		let result
			-- We've got enough information to try and crush the effect.
			| Just NCon{}		<- classUnified clsCon
			, Just _		<- classUnified clsArg
			= crushEffectApp cid (nApp, src) clsCon clsArg
			
			-- The constructor arg hasn't been unfied yet, but the effect
			-- might still be crushable later.
			| Just nCon@NCon{}	<- classUnified clsCon
			, elem nCon [nHeadRead, nDeepRead, nDeepWrite]
			= return CrushMaybeLater
			
			-- This effect will never be crushable.
			| otherwise
			= return CrushNever
			
			
		result
	
	-- Constructors and sums can't be crushed.
	| _	<- node
	= return CrushNever


-- | Try to crush an effect application.
crushEffectApp
	:: ClassId		-- ^ The cid of the class being crushed.
	-> (Node, TypeSource)	-- ^ The particular node constraint being crushed now.
	-> Class		-- ^ Class containing the constructor of the effect application.
	-> Class		-- ^ Class containing the argument of the effect application.
	-> SquidM CrushResult

crushEffectApp cid (nApp, srcApp) 
	         Class { classUnified = Just nCon  }
	clsArg	@Class { classUnified = Just nArg' }
 = do	
	nArg	<- sinkCidsInNode nArg'

	trace	$ vcat
	 	[ "    nApp             = " % nApp
		, "    nCon             = " % nCon
		, "    nArg             = " % nArg ]
		
	crushEffectApp' cid  (nApp, srcApp) clsArg nCon nArg

crushEffectApp _ _ _ _
	= panic stage $ "crushEffectApp: no match"
	

crushEffectApp' cid (nApp, srcApp) clsArg nCon nArg

	-- Effects on single constructors.
	--	When we do a case match on a unit value we get an effect HeadRead (),
	--	but () doesn't have a head region, so we can just drop the effect. 
	--	Likewise for other deep effects.
	| elem nCon [nHeadRead, nDeepRead, nDeepWrite]
	, NCon{}	<- nArg
	= return CrushBottom
		
	-- HeadRead ---------------------------------------
	| nCon		== nHeadRead
	, NApp{}	<- nArg
	= do	-- Start from the class holding the argument, 
		--	and walk down its left spine to get its head region, if any.
		mHead	<- takeHeadDownLeftSpine (classId clsArg)
		case mHead of
		 Just clsHead
		  -> do	let src	= TSI $ SICrushedES cid nApp srcApp
			cidRead	<- feedType src tRead
			return	$ CrushSimplified [(NApp cidRead (classId clsHead), src)]

		 Nothing 
		  -> 	return	CrushMaybeLater

	| nCon		== nHeadRead
	= return CrushMaybeLater


	-- DeepRead ---------------------------------------
	| nCon		== nDeepRead
	, NApp{}	<- nArg
	= do	mCidsApps <- takeAppsDownLeftSpine (classId clsArg)
		mClsApps  <- case mCidsApps of
				Nothing		-> return Nothing
				Just cids	-> liftM sequence $ mapM lookupClass cids
	
		let readIt src cls
			| classKind cls == kRegion	
			= do	cidRead	<- feedType src tRead
				return	$ Just (NApp cidRead (classId cls), src)
				
			| classKind cls == kValue	
			= do	cidDeepRead <- feedType src tDeepRead
				return	$ Just (NApp cidDeepRead (classId cls), src)

			| otherwise
			= 	return Nothing
		
		-- If the effect uses an abstract constructor like (ReadT (t a)) then we can't
		-- crush it yet. We can't build (ReadT t) because t has the wrong kind. We also
		-- can't crush to (ReadT a) because if t turns into to something like (List %r1),
		-- then we'll loose the effect on %r1.
		case mClsApps of
		 Just (clsCon : clsArgs)
		  | Just NCon{}	<- classUnified clsCon
		  -> do	let src	  = TSI $ SICrushedES cid nApp srcApp
			nodeSrcs' <- liftM catMaybes $ mapM (readIt src) clsArgs
			return	  $ CrushSimplified nodeSrcs'
			
		 _ ->	return	CrushMaybeLater
	
	| nCon		== nDeepRead
	= return CrushMaybeLater


	-- DeepWrite --------------------------------------
	| nCon		== nDeepWrite
	, NApp{}	<- nArg
	= do	mCidsApps <- takeAppsDownLeftSpine (classId clsArg)
		mClsApps  <- case mCidsApps of
				Nothing		-> return Nothing
				Just cids	-> liftM sequence $ mapM lookupClass cids
	
		let writeIt src cls
			| classKind cls == kRegion	
			= do	cidWrite <- feedType src tWrite
				return	$ Just (NApp cidWrite (classId cls), src)
				
			| classKind cls	== kValue
			= do	cidDeepWrite <- feedType src tDeepWrite
				return	$ Just (NApp cidDeepWrite (classId cls), src)
				
			| otherwise
			= 	return Nothing

		case mClsApps of
		 Just (clsCon : clsArgs)
		  | Just NCon{} <- classUnified clsCon
		  -> do	let src	  = TSI $ SICrushedES cid nApp srcApp
			nodeSrcs' <- liftM catMaybes $ mapM (writeIt src) clsArgs
			return	  $ CrushSimplified nodeSrcs'
			
		 _ ->	return	CrushMaybeLater
			
	| nCon		== nDeepWrite
	= return CrushMaybeLater


	-- Other effects can never be crushed.			
	| otherwise
	= return CrushNever

