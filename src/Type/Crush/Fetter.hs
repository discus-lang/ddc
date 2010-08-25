{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crushing of built-in single parameter type class (SPTC) constraints
--	like Pure, HeadLazy, DeepConst, DeepMutable.
module Type.Crush.Fetter
	(crushFettersInClass)
where
import DDC.Type
import DDC.Solve.State
{-
import Type.State
import Type.Class
import Type.Location
import Type.Feed
import Type.Error
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Solve.Walk
import DDC.Type
import Data.Maybe
import Shared.VarPrim
import Control.Monad
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Data.Sequence	as Seq

stage	= "Type.Crush.Fetter"
debug	= True
trace s	= when debug $ traceM s
-}
-- | Try and crush any single parameter fetters acting on this
--	class into smaller components.
crushFettersInClass
	:: ClassId 	-- ^ cid of class containing the fetters to crush.
	-> SquidM Bool	-- ^ Whether we crushed something from this class.

crushFettersInClass cid
 = do	return False
	
{-	Just cls <- lookupClass cid
 	crushFetterWithClass cid cls
-}
{-
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
		, classFetters		= fetterSrcs }
	 -> do	
		nNode	<- sinkCidsInNode	    nNode'
		tsSrc	<- mapM sinkCidsInNodeFst   tsSrc'

		let fsSrc	
			= [(FConstraint v [TVar kind $ UClass cid], src)
				| (v, srcs)		<- Map.toList fetterSrcs
				, let src Seq.:< _	= Seq.viewl srcs]

		trace	$ "--  crushFetterInClass "	%  cid		% "\n"
			% "    node           = "	%  nNode	% "\n"
			% "    fetters:\n" 		%> fetterSrcs	% "\n"

		-- Try to crush each fetter into smaller pieces.
		progress	<- liftM or
				$ mapM (crushFetterSingle cid cls nNode) fsSrc
		
		return False

-- | Try to crush a fetter from a class into smaller pieces.
--	All parameters should have their cids canonicalised.
crushFetterSingle
	:: ClassId				-- ^ The cid of the class this fetter is from.
	-> Class				-- ^ The class containing the fetter.
	-> Node					-- ^ The node type of the class.
	-> (Fetter, TypeSource)			-- ^ The var and source of the fetter to crush.
	-> SquidM Bool				-- ^ Whether we made progress.
	
crushFetterSingle cid cls node 
	fsrc@(fetter@(FConstraint vFetter _), srcFetter)

	-- HeadLazy ---------------------------------------
	| vFetter == primLazyH
	= do	mclsHead <- takeHeadDownLeftSpine cid
		case mclsHead of
			Just clsHead	
			 -> do	deleteSingleFetter cid vFetter

				let src		= TSI $ SICrushedFS cid fetter srcFetter
				let tHead	= TVar (classKind clsHead) $ UClass (classId clsHead)
				let headFetter	= FConstraint primLazy [tHead]
				addFetter src headFetter

				trace	$ vcat
					[ ppr "  * crushing LazyH\n"
					, "    headFetter = " % headFetter ]
				
				return True
				
			_ -> return False

	-- DeepConst --------------------------------------
	| vFetter == primConstT
	= do	trace	$ ppr "  * crushing ConstT\n"
		mApps	<- takeAppsDownLeftSpine cid
	
		let constIt (cid', k)
			| k == kRegion	= Just $ FConstraint primConst  [TVar k $ UClass cid']
			| k == kValue	= Just $ FConstraint primConstT [TVar k $ UClass cid']
			| otherwise	= Nothing
	
		case mApps of
		 Just (cidCon : cidArgs)
		  -> do	ksArgs		<- mapM kindOfCid cidArgs
			let cidksArgs	= zip cidArgs ksArgs
			let fs		= mapMaybe constIt cidksArgs
			let src		= TSI $ SICrushedFS cid fetter srcFetter
			zipWithM addFetter (repeat src) fs
			return False
			
		 Nothing	
		  -> return False
		
	
	-- DeepMutable ------------------------------------
	| vFetter == primMutableT
	= do	trace	$ ppr "  * crushing MutableT\n"
		mApps	<- takeAppsDownLeftSpine cid
	
		let mutableIt (cid', k)
			| k == kRegion	= Just $ FConstraint primMutable  [TVar k $ UClass cid']
			| k == kValue	= Just $ FConstraint primMutableT [TVar k $ UClass cid']
			| otherwise	= Nothing
	
		case mApps of
		 Just (cidCon : cidArgs)
		  -> do	ksArgs		<- mapM kindOfCid cidArgs
			let cidksArgs	= zip cidArgs ksArgs
			let fs		= mapMaybe mutableIt cidksArgs
			let src		= TSI $ SICrushedFS cid fetter srcFetter
			zipWithM addFetter (repeat src) fs
			return False
			
		 Nothing	
		  -> return False


	-- Pure -------------------------------------------
	-- Apply the same constraint to all the cids in a sum.
	| vFetter == primPure
	, NSum cids	<- node
	= do	let cidsList	= Set.toList cids
		ks		<- mapM kindOfCid cidsList
		let ts		= zipWith (\k c -> TVar k (UClass c)) ks cidsList
		zipWithM addFetter
			(repeat $ TSI $ SICrushedFS cid fetter srcFetter)
			[FConstraint primPure [t] | t <- ts]

		trace $ "  * Sum " % ts % "\n"

		return True

	 -- When crushing purity fetters we must leave the original constraint in the graph.
	| vFetter == primPure
	, isNApp node || isNCon node
	= do	-- Get the fetter that purifies this one, if any.
		ePurifier 	<- getPurifier cid cls node fetter srcFetter
		case ePurifier of
		 Left err 
		  -> do	addErrors [err]
			return False
		
		 Right (Just (fPurifier, srcPurifier))
		  -> do	addFetter srcPurifier fPurifier
			return True
							
		 Right Nothing
		  -> 	return False
			
	| vFetter == primPure
	= return False

	-- Some other Fetter ------------------------------
	| otherwise
	= return False



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
		let tsArgs	= [TVar (classKind c) $ UClass (classId c) | c <- clsArgs]
		
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
		, TSI $ SIPurifier cid (makeTApp tRead tsArgs) srcEff 
				fetter srcFetter)

	-- DeepRead is purified by DeepConst
	| classType clsCon == Just nDeepRead
	, [_]	<- clsArgs
	= Right $ Just 
		( FConstraint primConstT tsArgs
		, TSI $ SIPurifier cid (makeTApp tDeepRead tsArgs) srcEff 
				fetter srcFetter)
	
	-- We don't have a HeadConst fetter, but as all HeadReads are guaranteed to be
	-- crushed into regular Reads we can just wait until that happens.
	| classType clsCon == Just nHeadRead
	, [_]	<- clsArgs
	= Right Nothing 
	
	-- This effect can't be purified.
	| Just nCon@(NCon tc)	<- classType clsCon
	= Left 	$ ErrorCannotPurify
		{ eEffect		= makeTApp (TCon tc) tsArgs
		, eEffectSource		= srcEff
		, eFetter		= fetter
		, eFetterSource		= srcFetter }
-}
