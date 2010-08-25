{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crush\/simplify compound effects into their parts.
module Type.Crush.Effects
	(crushEffectsInClass)
where
import DDC.Type
import Control.Monad
import DDC.Solve.State
import DDC.Main.Pretty

{-
import Type.Location
import Type.Class
import Type.Feed
import DDC.Main.Error
import DDC.Solve.Walk
import Data.Maybe
import qualified Data.Set	as Set
-}
debug	= False
trace s	= when debug $ traceM s
-- stage	= "Type.Crush.Effects"

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
	-> SquidM Bool		-- ^ Whether we crushed something from this class.

crushEffectsInClass cid
 = do	trace		$ "--  crushEffectInClass " % cid % "\n"
	return False
	
{-
	Just cls	<- lookupClass cid
	crushEffectWithClass cid cls
	
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
	 -> crushEffectInClass cid'
		
	-- Class hasn't been unified yet.
	Class	{ classType = Nothing }
	 -> crushMaybeLater cid
	
	-- A class containing an application.
	Class	{ classKind = kind
		, classType = Just nApp'@NApp{} }
		
	 -> do	nApp@(NApp cidCon cidArg) <- sinkCidsInNode nApp'
		Just clsCon	<- lookupClass cidCon
		Just clsArg	<- lookupClass cidArg
		
		trace	$ vcat
			[ "    node             = "	% nApp 
			, "    clsCon.classType = "	% classType clsCon
			, "    clsArg.classType = "	% classType clsArg ]

		let result
			-- We've got enough information to try and crush the effect.
			| Just NCon{}	<- classType clsCon
			, Just _	<- classType clsArg
			= crushEffectApp cid cls clsCon clsArg
			
			-- Either the constructor or arg hasn't been unified yet.
			| otherwise
			= crushMaybeLater cid
			
		result
		
	-- Some other class
	Class	{ classType = Just _ }
	 -> crushNever cid
	

-- | Try and crush an effect application.
--   The classTypes in all the provided classes have been unified, so are Justs.
crushEffectApp
	:: ClassId		-- ^ cid of the class containing the outer application.
	-> Class		-- ^ Class containing the application.
	-> Class		-- ^ Class containing the constructor.
	-> Class		-- ^ Class containign the argument.
	-> SquidM Bool		-- ^ Whether we made any progress.

crushEffectApp cid 
	cls	@Class { classType = Just nApp' }
	clsCon	@Class { classType = Just nCon  }
	clsArg	@Class { classType = Just nArg' }
 = do	
	nApp	<- sinkCidsInNode nApp'
	nArg	<- sinkCidsInNode nArg'

	Just srcApp	<- lookupSourceOfNode nApp cls

	trace	$ vcat
	 	[ ppr "  * crushEffectApp..."
		, "    nApp             = " % nApp
		, "    nCon             = " % nCon
		, "    nArg             = " % nArg ]
		
	crushEffectApp' cid cls clsCon clsArg nApp srcApp nCon nArg

crushEffectApp' cid cls clsCon clsArg nApp srcApp nCon nArg

	-- Effects on single constructors.
	--	When we do a case match on a unit value we get an effect HeadRead (),
	--	but () doesn't have a head region, so we can just drop the effect. 
	--	Likewise for other deep effects.
	| elem nCon [nHeadRead, nDeepRead, nDeepWrite]
	, NCon{}	<- nArg
	= do	Just srcApp	<- lookupSourceOfNode nApp cls
		crushBottom cid
	
	-- HeadRead ---------------------------------------
	| nCon		== nHeadRead
	, NApp{}	<- nArg
	= do	-- Start from the class holding the argument, 
		--	and walk down its left spine to get its head region, if any.
		mHead	<- takeHeadDownLeftSpine (classId clsArg)
		case mHead of
		 Just clsHead
		  -> do	cidEff'	<- feedType (TSI $ SICrushedES cid nApp srcApp) 
				$  makeTApp tRead [TVar (classKind clsHead) $ UClass (classId clsHead)]

			crushUpdate cid 
				[cidEff']
				(TSI $ SICrushedES cid nApp srcApp)

		 Nothing	-> crushMaybeLater cid

	| nCon		== nHeadRead
	= crushMaybeLater cid

	-- DeepRead ---------------------------------------
	| nCon		== nDeepRead
	, NApp{}	<- nArg
	= do	mCidsApps <- takeAppsDownLeftSpine (classId clsArg)
		mClsApps  <- case mCidsApps of
				Nothing		-> return Nothing
				Just cids	-> liftM sequence $ mapM lookupClass cids
	
		let readIt cls
			| classKind cls == kRegion	= Just $ TApp tRead	tClass
			| classKind cls == kValue	= Just $ TApp tDeepRead	tClass
			| otherwise			= Nothing
			where Just tClass = takeTClassOfClass cls
		
		-- If the effect uses an abstract constructor like (ReadT (t a)) then we can't
		-- crush it yet. We can't build (ReadT t) because t has the wrong kind. We also
		-- can't crush to (ReadT a) because if t turns into to something like (List %r1),
		-- then we'll loose the effect on %r1.
		case mClsApps of
		 Just (clsCon : clsArgs)
		  | Just NCon{}	<- classType clsCon
		  -> do	let effs'	= mapMaybe readIt clsArgs
			let src		= TSI $ SICrushedES cid nApp srcApp
			cidsEff' <- mapM (feedType src) effs'			
			crushUpdate cid cidsEff' src
			
		 _ ->	crushMaybeLater cid
	
	| nCon		== nDeepRead
	= crushMaybeLater cid
			
	-- DeepWrite --------------------------------------
	| nCon		== nDeepWrite
	, NApp{}	<- nArg
	= do	mCidsApps <- takeAppsDownLeftSpine (classId clsArg)
		mClsApps  <- case mCidsApps of
				Nothing		-> return Nothing
				Just cids	-> liftM sequence $ mapM lookupClass cids
	
		let writeIt cls
			| classKind cls == kRegion	= Just $ TApp tWrite	 tClass
			| classKind cls	== kValue	= Just $ TApp tDeepWrite tClass
			| otherwise			= Nothing
			where Just tClass = takeTClassOfClass cls

		case mClsApps of
		 Just (clsCon : clsArgs)
		  | Just NCon{} <- classType clsCon
		  -> do	let effs'	= mapMaybe writeIt clsArgs
			let src		= TSI $ SICrushedES cid nApp srcApp
			cidsEff' <- mapM (feedType src) effs'			
			crushUpdate cid cidsEff' src
			
		 _ ->	crushMaybeLater cid
			
	| nCon		== nDeepWrite
	= crushMaybeLater cid

	-- Other effects can never be crushed.			
	| otherwise
	= crushNever cid


-- | Class is not yet crushable, nor will it ever be.
--   Maybe the class contains some already atomic effect, like Read or Write.
crushNever :: ClassId -> SquidM Bool
crushNever cid
 = do	trace	$ ppr "  * never crushable\n\n"
	return False


-- | The effect in this class was completely crushed out.
--   Maybe it was a (HeadRead ()), which has no effect.
--   We can set this class to bottom, but still need to retain it in the graph
--   because other classes may be holding references to it. 
--   We don't use crushUpdate here because that function also requires a TypeSource.
crushBottom :: ClassId -> SquidM Bool
crushBottom cid
 = do	trace	$ ppr "  * delete\n\n"
	modifyClass cid $ \c -> c
		{ classType		= Just NBot
		, classTypeSources	= [] }

	return False


-- | We haven't made any progress this time around, but the same class might
--   be crushable when some other class changes. Maybe some other class needs
--   to be unified to give us an outer type constructor.
--
--   TODO: Could use activation queues here to speed the grinder up.
--
crushMaybeLater :: ClassId -> SquidM Bool
crushMaybeLater cid
 = do	trace	$ ppr "  * maybe later\n\n"
	activateClass cid
	return False
	

-- | Class is crushable, and here are cids holding the new effects, 
---  along with a TypeSource recording what was crushed.
crushUpdate :: ClassId -> [ClassId] -> TypeSource -> SquidM Bool
crushUpdate cid cids' src'
 = do	trace	$ "  * update\n"
		% "    crushed effects:\n" %> cids' <> src' % "\n\n"

	case cids' of
	 [] ->	panic stage "crushEffectUpdate: List is empty. Should have called crushBottom."

	 -- If there's only one new cid then we can just fwd the original class to it.
	 [cid']	
	  -> do	modifyClass cid $ \c -> ClassForward cid cid'
		return True

         -- Crushing the orignal effect gave us a number of pieces,
	 -- so we join them together into an effect sum.
	 _ -> do
		let node	= NSum $ Set.fromList cids'
		modifyClass cid $ \c -> c
			{ classType		= Just node
			, classTypeSources	= [(node, src')] }
		return True
-}					
