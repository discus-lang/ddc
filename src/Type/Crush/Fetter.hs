{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crushing of built-in single parameter type class (SPTC) constraints
--	like Pure, HeadLazy, DeepConst, DeepMutable.
module Type.Crush.Fetter
	(crushFetterInClass)
where
import Type.State
import Type.Exp
import Type.Class
import Type.Location
import Type.Feed
import Type.Error
import Type.Builtin
import Type.Util.Bits
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Solve.Walk
import Shared.VarPrim
import Control.Monad
import Util
import qualified Data.Set	as Set

stage	= "Type.Crush.Fetter"
debug	= True
trace s	= when debug $ traceM s

-- | Try and crush any single parameter fetters acting on this
--	class into smaller components.
crushFetterInClass
	:: ClassId 	-- ^ cid of class containing the fetters to crush.
	-> SquidM Bool	-- ^ Whether we crushed something from this class.

crushFetterInClass cid
 = do	Just cls <- lookupClass cid
 	crushFetterWithClass cid cls

crushFetterWithClass cid cls
 = case cls of
	ClassUnallocated 
	 -> panic stage $ "crushFetterWithClass: ClassUnallocated"
	
	-- Follow indirections.
	ClassForward cid cid'
	 -> crushFetterInClass cid'

	-- MPTC style fetters Shape and Proj are handled by their own modules.
	ClassFetterDeleted{}	-> return False
	ClassFetter{}		-> return False

	-- Class hasn't been unified yet.
	Class 	{ classType = Nothing }
	 -> return False

	Class	{ classKind		= kind
		, classType		= Just nNode'
		, classTypeSources	= tsSrc'
		, classFetterSources 	= fsSrc' }
	 -> do	
		nNode	<- sinkCidsInNode	    nNode'
		tsSrc	<- mapM sinkCidsInNodeFst   tsSrc'
		fsSrc	<- mapM sinkCidsInFetterFst fsSrc'

		trace	$ "--  crushFetterInClass "	%  cid		% "\n"
			% "    node           = "	%  nNode	% "\n"
			% "    fetters:\n" 		%> fsSrc	% "\n"

		-- Try to crush each fetter into smaller pieces.
		-- While crushing, we leave all the original fetters in the class, and only add
		-- the new fetters back when we're done. The fetters in the returned list could
		-- refer to other classes as well as this one.
		mfsCrushed 	<- mapM (crushFetterSingle cid cls nNode) fsSrc
		let progress	= or $ map isJust mfsCrushed
		
		-- For each fetter, it crushed we get Just and a new list of fetters.
		-- If it didn't crush we get a Nothing, so we want to keep the original.
		let fsCrushed	= concat
				$ zipWith (\fsOrig fsCrush -> fromMaybe [fsOrig] fsCrush)
					fsSrc
					mfsCrushed
						
		when progress
		 $ trace $ "    crushed fetters:\n"	%> vcat fsCrushed 
			 % newline

		-- Clear out the original fetters from the class.
		updateClass cid $ cls { classFetterSources	= [] }
	
		-- Add all the fetters back to the graph
		-- TODO: only count progress when we've add a fetter that wasn't there before.
		--       If we report progress the grinder will call us again on the same class.
		--	 Is this actually what we want?
		mapM (\(f, src) -> addFetter src f) fsCrushed

		return progress


-- | Try to crush a fetter from a class into smaller pieces.
--	All parameters should have their cids canonicalised.
crushFetterSingle
	:: ClassId				-- ^ The cid of the class this fetter is from.
	-> Class				-- ^ The class containing the fetter.
	-> Node					-- ^ The node type of the class.
	-> (Fetter, TypeSource)			-- ^ The fetter to crush
	-> SquidM 
		(Maybe [(Fetter, TypeSource)])	-- ^ If crushable then the new pieces, or `Nothing`
						--   if the original fetter is not crushable yet.
	
crushFetterSingle cid cls node
	fsrc@(fetter@(FConstraint vFetter tsFetter), srcFetter)

	-- HeadLazy
	| vFetter == primLazyH
	= do	trace	$ ppr "  * crushing LazyH\n"
		mclsHead <- takeHeadDownLeftSpine cid
		case mclsHead of
			Just clsHead	
			 -> do	let tHead	= TClass (classKind clsHead) (classId clsHead)
				let src		= TSI $ SICrushedFS cid fetter srcFetter
				return $ Just [(FConstraint primLazy [tHead], src)]
			
			_ -> return Nothing

	-- DeepConst
	
	-- DeepMutable

	-- Pure
	| vFetter == primPure
	= do	trace	$ vcat
			[ ppr "  * crushing Pure"
			, "    tsFetter       = " % tsFetter]

		case node of

		 -- Apply the same constraint to all the cids in a sum.
		 NSum cids
		  -> do	let cidsList	= Set.toList cids
			ks		<- mapM kindOfCid cidsList
			let ts		= zipWith TClass ks cidsList
			return	$ Just  
				$ zip 	[FConstraint primPure [t] | t <- ts]
					(repeat $ TSI $ SICrushedFS cid fetter srcFetter)

		 -- When crushing purity fetters we must leave the original constraint in the graph.
		 NApp{}
		  -> do	-- Get the fetter that purifies this one, if any.
			ePurifier 	<- getPurifier cid cls node fetter srcFetter
			case ePurifier of
			 Left err 
			  -> do	addErrors [err]
				return $ Just [fsrc]
			
			 Right (Just fsrcPurifier)
			  -> 	return $ Just [fsrc, fsrcPurifier]
							
			 Right Nothing
			  -> 	return $ Just [fsrc]
			
		 _ -> return Nothing

	| otherwise
	= return Nothing


-- | Get the fetter we need to add to the graph to ensure that the effect
--   in the given class is pure.
getPurifier
	:: ClassId		-- ^ Cid of the class containing the effect we want to purify.
	-> Class		-- ^ That class.
	-> Node			-- ^ The node type from the class.
	-> Fetter		-- ^ The fetter we want to purify.
	-> TypeSource		-- ^ Source of that fetter.
	-> SquidM 
		(Either Error (Maybe (Fetter, TypeSource)))
				-- ^ If the effect can't be purified left the error saying so.
				--   Otherwise, right the purifying fetter, if any is needed.

getPurifier cid cls nodeEff fetter srcFetter
 = do	-- See what sort of effect we're dealing with
	mCids	<- takeAppsDownLeftSpine cid
	case mCids of
	 Just (cidCon : cidArgs)
	  -> do	Just clsCon	<- lookupClass cidCon
		Just clsArgs	<- liftM sequence $ mapM lookupClass cidArgs
		let tsArgs	= [TClass (classKind c) (classId c) | c <- clsArgs]
		

		Just srcEff	 <- lookupSourceOfNode nodeEff cls
		let ePurifier	=  getPurifier' cid fetter srcFetter clsCon clsArgs tsArgs srcEff
		
		trace	$ vcat
			[ "  * getPurifier " 		% cid
			, "    clsCon.classType  = "	% classType clsCon
			, "    clsArgs.classType = "	% (map classType clsArgs)
			, "    purifier          = "	% ePurifier ]
		
		return ePurifier
		
	 _ ->	return $ Right Nothing
		
getPurifier' cid fetter srcFetter clsCon clsArgs tsArgs srcEff
	-- Read is purified by Const
	| classType clsCon == Just nRead
	, [_]	<- clsArgs
	= Right $ Just 	
		( FConstraint primConst tsArgs
		, TSI $ SIPurifier cid (makeTApp (tRead:tsArgs)) srcEff 
				fetter srcFetter)

	-- DeepRead is purified by DeepConst
	| classType clsCon == Just nDeepRead
	, [_]	<- clsArgs
	= Right $ Just 
		( FConstraint primConstT tsArgs
		, TSI $ SIPurifier cid (makeTApp (tDeepRead:tsArgs)) srcEff 
				fetter srcFetter)
	
	-- We don't have a HeadConst fetter, but as all HeadReads are guaranteed to be
	-- crushed into regular Reads we can just wait until that happens.
	| classType clsCon == Just nHeadRead
	, [_]	<- clsArgs
	= Right Nothing 
	
	| Just nCon@(NCon tc)	<- classType clsCon
	= Left 	$ ErrorCannotPurify
		{ eEffect		= makeTApp (TCon tc : tsArgs)
		, eEffectSource		= srcEff
		, eFetter		= fetter
		, eFetterSource		= srcFetter }




{-

-- | Crush a non-purity fetter that's constraining some node in the graph.
crushFetterSingle_fromGraph 
	:: ClassId			-- cid of class being constrained.
	-> Kind 
	-> Type				-- the node type being constrained
	-> Var				-- var of fetter ctor

	-> SquidM 			-- if Just [Fetters] then the original fetter is removed and these
					--			  new ones are added to the graph.
		(Maybe [Fetter])	--    Nothing        then leave the original fetter in the class.

crushFetterSingle_fromGraph cid k tNode vC
	-- lazy head
	| vC	== primLazyH
	= do	trace $ ppr "    -- crushing LazyH\n"
		mtHead	<- headTypeDownLeftSpine cid
		case mtHead of
			Just t	-> return $ Just [FConstraint primLazy [t]]
			_	-> return Nothing

	-- deep constancy
	| vC	== primConstT
	= do	trace $ ppr "    -- crushing deep constancy\n"
		case tNode of
		 TApp t1 t2
		  -> return 
		  $  Just [ FConstraint primConstT [t1]
			  , FConstraint primConstT [t2] ]

		 TCon{}	-> return $ Just []

		 -- Constraining a closure or effect to be mutable doesn't mean anything useful
		 TSum k []
		  | k == kRegion	-> return $ Just [ FConstraint primConst [TClass k cid] ]
		  | k == kClosure	-> return $ Just []
		  | k == kEffect	-> return $ Just []
		  | otherwise		-> return   Nothing

		 _ 	-> return $ Nothing

	-- deep mutability
	| vC	== primMutableT
	= do	trace $ ppr "    -- crushing MutableT\n"
		case tNode of
		 TApp t1 t2
		  -> return
		  $  Just [ FConstraint primMutableT [t1]
			  , FConstraint primMutableT [t2] ]
			
		 TCon{} -> return $ Just []

		 -- Constraining a closure or effect to be mutable doesn't mean anything useful.
		 TSum k []
		  | k == kRegion	-> return $ Just [ FConstraint primMutable [TClass k cid] ]
		  | k == kClosure	-> return $ Just []
		  | k == kEffect	-> return $ Just []
		  | otherwise		-> return   Nothing
		
		 _ 	-> return Nothing
	

	| otherwise
	= return Nothing
-}
