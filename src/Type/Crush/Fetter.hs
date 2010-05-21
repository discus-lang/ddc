{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crushing of built-in single parameter type class (SPTC) constraints
--	like Pure and LazyH.
--
module Type.Crush.Fetter
	(crushFetterInClass)
where
import Type.State
import Type.Exp
import Type.Class
import Type.Location
import Type.Feed
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Solve.Walk
import Shared.VarPrim
import Control.Monad
import Util

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
		mfsCrushed 	<- mapM (crushFetterSingle cid kind nNode tsSrc) fsSrc
		let progress	= or $ map isJust mfsCrushed
		
		-- For each fetter, it crushed we get Just and a new list of fetters.
		-- If it didn't crush we get a Nothing, so we want to keep the original.
		let fsCrushed	= concat
				$ zipWith (\fsOrig fsCrush -> fromMaybe [fsOrig] fsCrush)
					fsSrc
					mfsCrushed
						
		trace	$ "    crushed fetters:\n"	%> fsCrushed	% "\n\n"

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
	-> Kind				 	-- ^ The kind of the class.
	-> Node					-- ^ The node type in the class.
	-> [(Node, TypeSource)]			-- ^ Node constraints contributing to the class.
	-> (Fetter, TypeSource)			-- ^ The fetter to crush
	-> SquidM 
		(Maybe [(Fetter, TypeSource)])	-- ^ If crushable then the new pieces, or `Nothing`
						--   if the original fetter is not crushable yet.
	
crushFetterSingle cid kind node nsSrc 
	(fetter@(FConstraint vFetter tsFetter), srcFetter)

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

	-- Pure
	| vFetter == primPure
	= do	trace	$ vcat
			[ ppr "  * crushing Pure"
			, "    tsFetter = " % tsFetter]
		return Nothing
		


	| otherwise
	= return Nothing


{-
-- | Crush a purity constraint
crushFetterPure cid k tNode ts_src
	fPure_src@(FConstraint vC [tC@TClass{}], _)
 = do	
	trace	$ "    -- crushing purity constraint"	% "\n"

	-- flatten out the effect sum into individual atomic effects
	let effs_atomic = flattenTSum tNode
	trace	$ "    effs_atomic                    = " % effs_atomic % "\n"

	-- get the fetters of each atomic effect,
	mfsPurifiers		<- mapM purifyEffect_fromGraph effs_atomic
	trace	$ "    mfsPurifiers                   = " % mfsPurifiers % "\n"

	-- see if all the atomic effects could be purifier
	case sequence mfsPurifiers of
	 Just fsPurifiers	
	  -> let eff_fPurifiers	= zip effs_atomic fsPurifiers
	     in  crushFetterPure_success cid k tNode ts_src fPure_src eff_fPurifiers

	 Nothing		
	  -> crushFetterPure_failed cid k tNode ts_src fPure_src 
		(zip effs_atomic mfsPurifiers)

-- we have a new constraint that forces each of the atomic effects to be pure.
crushFetterPure_success cid k tNode ts_src
	fPure_src@( fPure@(FConstraint vC [tC@TClass{}]), fPureSrc )
	eff_fPurifiers
 = do	
	-- Make the the new TypeSource info for each of the new fetters
	let makePurifierSrc (eff, fPurifier)
	     = let -- lookup the type-source for this effect
		   --	The nodes hold effect sums, so we need to look inside them
		   --	to find which one holds our (atomic) conflicting effect
		   effSrc : _ 	
		    = [nodeEffSrc	
				| (nodeEff,  nodeEffSrc)	<- ts_src
				, elem eff $ flattenTSum nodeEff]
 
	
		   src	= TSI (SIPurifier cid eff effSrc fPure fPureSrc)
	       in  (fPurifier, src)	

	let fPurifiers_src
		= map makePurifierSrc eff_fPurifiers

	--	and make the source info for constraints added due to purification.
	trace	$ "    fPurifiers_src                  = " % fPurifiers_src % "\n"
		
	-- we've made progress on this class if any new fetters were added
	return	$ fPure_src : fPurifiers_src


-- one of the effects could not be purified
crushFetterPure_failed cid k tNode ts_src
	fPure_src@(fPure, fPureSrc) 
	eff_mPurifiers

 = do	trace	$ ppr "-- purification failed"

	-- lookup the first effect that we couldn't make a purifier for
	let badAtomicEff : _
		= [ eff	| (eff, Nothing)	<- eff_mPurifiers ]

	-- lookup the type-source for the conflicting effect
	--	The nodes hold effect sums, so we need to look inside them
	--	to find which one holds our (single) conflicting effect
	let badAtomicEff_src : _ 	
		= [srcEff	| (eff,  srcEff)	<- ts_src
				, elem badAtomicEff $ flattenTSum eff]

	-- build the purification error
	let err	= ErrorCannotPurify
			{ eEffect		= badAtomicEff
			, eEffectSource		= badAtomicEff_src
			, eFetter		= fPure
			, eFetterSource		= fPureSrc }

	-- add the error to the type inferencer monad.
	addErrors [err]

	-- report no progress, and keep the original class and fetter.
	return [ fPure_src ]


-- | Given an effect, produce the fetter which purifies it,
--	or Nothing if this is not possible.
--
purifyEffect_fromGraph
	:: Effect 
	-> SquidM (Maybe Fetter)

purifyEffect_fromGraph eff
 = case eff of
	TApp tE@(TClass _ cid) tArg
	 -> do	Just cls	<- lookupClass cid
		case classType cls of
		 Just tE'	-> purifyEffect_fromGraph' (TApp tE' tArg)
		 Nothing	-> return Nothing

	TApp tE tArg
	 -> purifyEffect_fromGraph' eff
		
	-- effect variable
	TClass kE cid
	 | kE	== kEffect
	 -> return $ Just $ FConstraint primPure [eff]

	_ -> return Nothing
	
purifyEffect_fromGraph' eff
	-- read
 	| TApp tE tR@(TClass kR _)	<- eff
	, tE	== tRead
	, kR	== kRegion
	= return $ Just $ FConstraint primConst [tR]

	-- head read
 	| TApp tE tR@(TClass kV cidV)	<- eff
	, tE 	== tHeadRead
	, kV	== kValue
	= do	mHeadType	<- headTypeDownLeftSpine cidV
			
		case mHeadType of
		 Just tR@(TClass kR _)
		  | kR == kRegion	-> return $ Just $ FConstraint primConst [tR]
		 _			-> return $ Nothing
		
	-- deep read
	-- TODO: Do we realy want ReadT to be able to operate
	--	 on types of any kind?
 	| TApp tE tR@(TClass kV _)	<- eff
--	, kV	== kValue
	, tE	== tDeepRead
	= return $ Just $ FConstraint primConstT [tR]
	

	| otherwise
	= return $ Nothing


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
