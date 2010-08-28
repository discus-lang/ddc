{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-incomplete-record-updates #-}

module DDC.Solve.Crush.Fetter
	(crushFettersInClass)
where
import DDC.Solve.State
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
debug	= False
trace s	= when debug $ traceM s

-- | Crushing of built-in single parameter type class (SPTC) constraints
--	like Pure, HeadLazy, DeepConst, DeepMutable.
crushFettersInClass
	:: ClassId 	-- ^ cid of class containing the fetters to crush.
	-> SquidM Bool	-- ^ Whether we crushed something from this class.

crushFettersInClass cid
 = do	Just cls <- lookupClass cid
 	crushFetterWithClass cid cls

crushFetterWithClass cid cls
 = case cls of
	ClassUnallocated 
	 -> panic stage $ "crushFetterWithClass: ClassUnallocated"
	
	-- Follow indirections.
	ClassForward _ cid'
	 -> crushFettersInClass cid'

	-- MPTC style fetters Shape and Proj are handled by their own modules.
	ClassFetterDeleted{}	-> return False
	ClassFetter{}		-> return False

	-- Try to crush SPTCs in a class.
	Class	{ classKind		= kind
		, classFetters		= fetterSrcs }
	 -> do	
		let fsSrc	
			= [(FConstraint v [TVar kind $ UClass cid], src)
				| (v, srcs)		<- Map.toList fetterSrcs
				, let src Seq.:< _	= Seq.viewl srcs]

		trace	$ "--  crushFetterInClass "	%  cid		% "\n"
			% "    fetters:\n" 		%> fetterSrcs	% "\n"

		-- Try to crush each fetter into smaller pieces.
		mapM_ (crushFetterSingle cid cls) fsSrc
		
		return False

-- | Try to crush a fetter from a class into smaller pieces.
--	All parameters should have their cids canonicalised.
crushFetterSingle
	:: ClassId				-- ^ The cid of the class this fetter is from.
	-> Class				-- ^ The class containing the fetter.
	-> (Fetter, TypeSource)			-- ^ The var and source of the fetter to crush.
	-> SquidM Bool				-- ^ Whether we made progress.
	
crushFetterSingle cid cls
	(fetter@(FConstraint vFetter _), srcFetter)

	-- HeadLazy ---------------------------------------
	| vFetter == primLazyH
	= do	mclsHead <- takeHeadDownLeftSpine cid
		case mclsHead of
			Just clsHead	
			 -> do	delSingleFetter cid vFetter

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
		 Just (_ : cidArgs)
		  -> do	ksArgs		<- mapM kindOfClass cidArgs
			let cidksArgs	= zip cidArgs ksArgs
			let fs		= mapMaybe constIt cidksArgs
			let src		= TSI $ SICrushedFS cid fetter srcFetter
			zipWithM addFetter (repeat src) fs
			return False
			
		 _ -> return False
		
	
	-- DeepMutable ------------------------------------
	| vFetter == primMutableT
	= do	trace	$ ppr "  * crushing MutableT\n"
		mApps	<- takeAppsDownLeftSpine cid
	
		let mutableIt (cid', k)
			| k == kRegion	= Just $ FConstraint primMutable  [TVar k $ UClass cid']
			| k == kValue	= Just $ FConstraint primMutableT [TVar k $ UClass cid']
			| otherwise	= Nothing
	
		case mApps of
		 Just (_ : cidArgs)
		  -> do	ksArgs		<- mapM kindOfClass cidArgs
			let cidksArgs	= zip cidArgs ksArgs
			let fs		= mapMaybe mutableIt cidksArgs
			let src		= TSI $ SICrushedFS cid fetter srcFetter
			zipWithM addFetter (repeat src) fs
			return False
			
		 _ -> return False


	-- Pure -------------------------------------------
	-- Apply the same constraint to all the cids in a sum.
	| vFetter == primPure
	= do	progress	
			<- mapM (purifyNodeOfClass cid fetter srcFetter)
			$  classTypeSources cls
			
		return	$ or progress
			

	-- Some other Fetter ------------------------------
	| otherwise
	= return False


-- | Purify an effect node in a class.
--   NOTE: We leave purified effects in the class.
purifyNodeOfClass 
	:: ClassId		-- ^ Cid containing the class being purified.
	-> Fetter		-- ^ The Pure fetter during doing the purification.
	-> TypeSource		-- ^ Source of the Pure Fetter.
	-> (Node, TypeSource)	-- ^ The effect that's being purified
	-> SquidM Bool		-- ^ Whether we changed the graph at all.

purifyNodeOfClass cid fPure srcPure (node, srcNode)
	| NSum cids	<- node
	= do	
		let cidsList	= Set.toList cids
		ks		<- mapM kindOfClass cidsList
		let ts		= zipWith (\k c -> TVar k (UClass c)) ks cidsList
		zipWithM addFetter
			(repeat $ TSI $ SICrushedFS cid fPure srcPure)
			[FConstraint primPure [t] | t <- ts]

		return True
	
	| isNApp node || isNCon node
	= do	
		-- Get the fetter that purifies this node, if any.
		ePurifier 	<- getPurifier cid fPure srcPure (node, srcNode)
		case ePurifier of
		 Left err 
		  -> do	addErrors [err]
			return False
		
		 Right (Just (fPurifier, srcPurifier))
		  -> do	addFetter srcPurifier fPurifier
			return True
							
		 Right Nothing
		  -> 	return False
	
	| otherwise
	= return False


-- | Get the fetter we need to add to the graph to ensure that the effect
--   in the given class is pure.
getPurifier
	:: ClassId		-- ^ Cid of the class containing the effect we want to purify.
	-> Fetter		-- ^ The Pure fetter.
	-> TypeSource		-- ^ Source of that fetter.
	-> (Node, TypeSource)	-- ^ The node type from the class.
	-> SquidM 
		(Either Error (Maybe (Fetter, TypeSource)))
				-- ^ If the effect can't be purified left the error saying so.
				--   Otherwise, right the purifying fetter, if any is needed.

getPurifier cid fetter srcFetter (nodeEff, srcNode)
	| NApp cidCon cidArg	<- nodeEff
	= do	Just clsCon	<- lookupClass cidCon
		Just clsArg	<- lookupClass cidArg
		let  tArg	= TVar (classKind clsArg) $ UClass (classId clsArg)
		return	$ getPurifier' cid fetter srcFetter clsCon tArg srcNode

	-- This effect can't be purified.
	| NCon tc	<- nodeEff
	= do	return 	$ Left $ ErrorCannotPurify
			{ eEffect		= TCon tc
			, eEffectSource		= srcNode
			, eFetter		= fetter
			, eFetterSource		= srcFetter }
		
	| otherwise
	= return $ Right Nothing
		

getPurifier' cid fetter srcFetter clsCon tArg srcNode
	-- Read is purified by Const
	| classUnified clsCon == Just nRead
	= Right $ Just 	
		( FConstraint primConst [tArg]
		, TSI $ SIPurifier cid 
				(makeTApp tRead [tArg]) srcNode
				fetter srcFetter)

	-- DeepRead is purified by DeepConst
	| classUnified clsCon == Just nDeepRead
	= Right $ Just 
		( FConstraint primConstT [tArg]
		, TSI $ SIPurifier cid
				(makeTApp tDeepRead [tArg]) srcNode
				fetter srcFetter)
	
	-- We don't have a HeadConst fetter, but as all HeadReads are guaranteed to be
	-- crushed into regular Reads we can just wait until that happens.
	| classUnified clsCon == Just nHeadRead
	= Right Nothing 
	
	-- This effect can't be purified.
	| Just (NCon tc)	<- classUnified clsCon
	= Left $ ErrorCannotPurify
		{ eEffect		= makeTApp (TCon tc) [tArg]
		, eEffectSource		= srcNode
		, eFetter		= fetter
		, eFetterSource		= srcFetter }

	| otherwise
	= panic stage $ "getPurifier: no match"