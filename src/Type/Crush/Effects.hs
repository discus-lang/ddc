{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crush effects into their parts.
module Type.Crush.Effects
	(crushEffectInClass)
where
import Type.Exp
import Type.Builtin
import Type.Location
import Type.Class
import Type.State
import Type.Util
import Type.Feed
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Solve.Walk
import Control.Monad
import Data.Maybe
import qualified Data.Set	as Set

debug	= True
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
	:: ClassId		-- ^ cid of the class containing the top level application.
	-> SquidM Bool		-- ^ Whether we crushed something from this class.

crushEffectInClass cid
 = do	Just cls	<- lookupClass cid
	crushEffectWithClass cid cls
	
crushEffectWithClass cid cls
 = case cls of
	ClassNil
	 -> return False
	
	-- Follow indirections.
	ClassForward cid'
	 -> crushEffectInClass cid'
	
	-- This isn't an effect class.
	-- The grinder shouldn't be calling us here.
	ClassFetter{}
	 -> panic stage
		$ "crushEffectWithClass: Class " % cid 
		% " is a ClassFetter{}, and doesn't contain an effect."
		
	-- Class hasn't been unified yet.
	Class	{ classType = Nothing }
	 -> return False
	
	-- A class containing an application.
	Class	{ classKind = kind
		, classType = Just nApp'@NApp{} }
		
	 -> do	nApp@(NApp cidCon cidArg) <- sinkCidsInNode nApp'
		Just clsCon	<- lookupClass cidCon
		Just clsArg	<- lookupClass cidArg
		
		trace	$ vcat
			[ "--  crushEffectInClass "	% cid
			, "    node             = "	% nApp 
			, "    clsCon.classType = "	% classType clsCon
			, "    clsArg.classType = "	% classType clsArg
			, blank ]

		let result
			-- We've got enough information to try and crush the effect.
			| Just NCon{}	<- classType clsCon
			, Just _	<- classType clsArg
			= crushEffectApp cid cls clsCon clsArg
			
			-- Can't crush this effect yet, but it looks like we would be able to
			-- if the argument was unified. Reactivate this class so we get called
			-- again on the next crushing pass.
			| Just nCon@NCon{} <- classType clsCon
			, Nothing	   <- classType clsArg
			, elem nCon [nHeadRead, nDeepRead, nDeepWrite]
			= do	activateClass cid
				trace	$ ppr "    * reactivating class\n"
				return False
				
			-- Either the constructor or arg hasn't been unified yet.
			| otherwise
			= do	trace	$ ppr "\n\n"
				return False
			
		result
		
	-- Some other class
	Class	{ classType = Just _ }
	 -> return False
	


data CrushResult
	= CrushNot
	| CrushGone
	| CrushBits [ClassId] TypeSource
	deriving Show

instance Pretty CrushResult PMode where
 ppr xx	
  = case xx of
	CrushNot		-> ppr "CrushNot"
	CrushGone		-> ppr "CrushGone"
	CrushBits cids src	-> "CrushBits" <> cids <> src

	
-- | Try and crush an effect application.
--   The classTypes in all the provided classes have been unified, so are Justs.
crushEffectApp
	:: ClassId		-- ^ cid of the class containing the outer application.
	-> Class		-- ^ Class containing the application.
	-> Class		-- ^ Class containing the constructor.
	-> Class		-- ^ Class containign the argument.
	-> SquidM Bool
				-- ^ Just the crushed parts of this effect, or Nothing
				--   if it couldn't be crushed.

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
		, "    nArg             = " % nArg 
		, blank ]
		
	crushEffectApp' cid cls clsCon clsArg nApp srcApp nCon nArg

crushEffectApp' cid cls clsCon clsArg nApp srcApp nCon nArg

	-- Effects on single constructors.
	--	When we do a case match on a unit value we get an effect HeadRead (),
	--	but () doesn't have a head region, so we can just drop the effect. 
	--	Likewise for other deep effects.
	| elem nCon [nHeadRead, nDeepRead, nDeepWrite]
	, NCon{}	<- nArg
	= do	Just srcApp	<- lookupSourceOfNode nApp cls
		crushEffectUpdate cid CrushGone
	
	-- HeadRead
	| nCon		== nHeadRead
	, NApp{}	<- nArg
	= do	
		-- Start from the class holding the argument, 
		--	and walk down its left spine to get its head region, if any.
		mHead	<- headClassDownLeftSpine (classId clsArg)
		case mHead of
		 Just clsHead
		  -> do	cidEff'	<- feedType (TSI $ SICrushedES cid nApp srcApp) 
				$  makeTApp [tRead, TClass (classKind clsHead) (classId clsHead)]

			crushEffectUpdate cid 
				(CrushBits [cidEff']
					   (TSI $ SICrushedES cid nApp srcApp))
			
		 Nothing	-> return False
			
	| otherwise
	= return False


crushEffectUpdate
	:: ClassId
	-> CrushResult
	-> SquidM Bool
	
crushEffectUpdate cid result
 = do	trace	$ "    crushed effects:\n" %> result % "\n\n"
	case result of
	 CrushNot	-> return False

	 CrushGone	
	  -> do	modifyClass cid $ \c -> c
			{ classType		= Just $ NSum Set.empty
			, classTypeSources	= [] }
		return True
		

	 CrushBits cids src
	  -> case cids of
		[] ->	panic stage "crushEffectUpdate: List is empty. Caller should have returned False."

		[cid']	
		 -> do	modifyClass cid $ \c -> ClassForward cid'
			return True

		_ -> do
			let node	= NSum $ Set.fromList cids
			modifyClass cid $ \c -> c
				{ classType		= Just node
				, classTypeSources	= [(node, src)] }
			return True
			

-- | Get the source of some effect, given the class that contains it.
--	The cids in the provided effect must be in canonical form, 
--	but the cids in the class don't need to be.
--	If there are multiple sources in the class then just take the first one.
lookupSourceOfNode
	:: Node
	-> Class 
	-> SquidM (Maybe TypeSource)

lookupSourceOfNode nEff cls
 = do	tsSrcs	<- mapM sinkCidsInNodeFst $ classTypeSources cls

	-- lookup the source of this effect
	--	The nodes hold effect sums, so we need to look inside them
	--	to find which one holds our (atomic) conflicting effect
	let srcs
	     = [nodeSrc	| (nodeEff,  nodeSrc)	<- tsSrcs
			, nodeEff == nEff]

	return $ listToMaybe srcs
	
{-
crushEffect cid cls nApp nAppSrc nEff nArg cidArg

	-- HeadRead
	| nEff		== tHeadRead
	, NApp{}	<- nArg
	= do	
		-- Start from the class holding the argument, 
		--	and walk down its left spine to get its head region, if any.
		mHead	<- headCidDownLeftSpine cidArg
		
		case mHead of
		 Just cidR	-> return $ Just
					( TApp tRead (TClass kRegion cidR)
					, TSI $ SICrushedES cid nApp nAppSrc )
			
		 Nothing	-> return Nothing
			
		
	-- DeepRead
	| nEff			== nDeepRead
	, NApp cid1 cid2	<- nArg
	= do	
		let readIt (k, cid)
			| k == kRegion	= Just $ TApp tRead 	 (TClass k cid)
			| k == kValue	= Just $ TApp tDeepRead (TClass k cid)
			| k == kClosure	= Nothing
			| k == kEffect	= Nothing
			
			-- TODO: tDeepRead doesn't really have this kind.
			| otherwise	= Just $ TApp tDeepRead (TClass k cid)
					
		Just k1		<- kindOfCid cid1
		Just k2		<- kindOfCid cid2
		let bits	= catMaybes $ map readIt [(k1, cid1), (k2, cid2)]
		
		return	$ Just
			( makeTSum kEffect bits
			, TSI $ SICrushedES cid nApp nAppSrc )


	-- DeepWrite
	| nEff			== nDeepWrite
	, TApp cid1 cid2	<- nArg
	= do	
		let writeIt (k, cid)
			| k == kRegion	= Just $ TApp tWrite 	  (TClass k cid)
			| k == kValue	= Just $ TApp tDeepWrite (TClass k cid)
			| k == kClosure	= Nothing
			| k == kEffect	= Nothing
			
			-- TODO: tDeepWrite doesn't really have this kind.
			| otherwise	= Just $ TApp tDeepWrite (TClass k cid)

		Just k1		<- kindOfCid cid1
		Just k2		<- kindOfCid cid2
		let bits	= catMaybes $ map writeIt [(k1, cid1), (k2, cid2)]
			
		return	$ Just
			( makeTSum kEffect bits
			, TSI $ SICrushedES cid nApp nAppSrc )
	
	
	| otherwise
	= do	
		-- Effect is still crushable.
		when (elem nEff [nHeadRead, nDeepRead, nDeepWrite])
		 $ do	trace $ ppr "reactivating\n"
			activateClass cid

		return Nothing
	

	
	
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


-- We've crushed the effect. Now update the class.
crushEffectWithClass_update cid cls Nothing
 = 	return False

	
crushEffectWithClass_update cid cls (Just (eff', src'))
 = do	trace	$ vcat
		[ "    effCrushed      = "	% eff'
		, "    srcCrushed      = "	% src'
		, newline ]

	-- Add the new effect to the graph.
	Just tt'	<- feedType src' Nothing eff'

	-- Update the original class to point to our new effect.
	modifyClass cid
	 $ \c -> c 	{ classType  	   = Just eff' 
		 	, classTypeSources = (eff', src') : classTypeSources c }

	-- If the crushed effect still contains crushable components then activate ourselves
	--	again so we get called on the next pass
	when (isCrushable eff') $ activateClass cid

	return True

-}

