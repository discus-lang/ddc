{-# OPTIONS -fno-warn-incomplete-record-updates #-}

-- | Crushing of built-in single parameter type class (SPTC) constraints
--	like Pure and LazyH.
--
module Type.Crush.Fetter
	( crushFetterC )

where
import Type.Diagnose
import Type.Feed
import Type.Trace
import Type.State
import Type.Util
import Type.Class
import Type.Location
import Type.Exp
import Type.Dump
import Type.Error

import Util
import Shared.Error
import Shared.VarPrim
import Shared.Var		(VarBind, NameSpace(..))
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var

import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Crush.Fetter"


-- | Try and crush any single parameter fetters acting on this
--	class into smaller components.
crushFetterC 
	:: ClassId 	-- ^ cid of class
	-> SquidM Bool	--   whether we crushed something from this class

crushFetterC cid
 = do	Just c	<- lookupClass cid
 	crushFetterC2 cid c

-- follow class indirections.
crushFetterC2 cid (ClassForward cid')
	= crushFetterC cid'

-- MPTC style fetters Shape and Proj are handled by their own modules.
crushFetterC2 cid (ClassFetter { classFetter = f })
	= return False

crushFetterC2 cid 
	cls@(Class	
		{ classKind	= k
		, classType	= Just tNode
		, classFetters 	= fs })
 = do	
	trace	$ "*   crushFetterC "   % k % cid			% "\n"
		% "    node type    (tNode)           = " % tNode	% "\n"
		% "    node fetters (fs)              = " % fs		% "\n"

	-- try to crush each fetter in turn, into smaller bits.
	--	crushing a particular fetter (like Pure) might also cause
	--	the type in the node to change (such as by removing an effect term)
	(cls', progressFetters)	
		<- mapAccumLM crushFetterSingle cls
		$  fs
	
	let (progresss, mfsCrushed)
			= unzip progressFetters

	let fsCrushed	= catMaybes mfsCrushed
	
	trace	$ "    progress                       = " % progresss		% "\n"
		% "    new node type     (tNode')     = " % classType cls'	% "\n"
		% "    new node fetters  (fsCrushed)  = " % fsCrushed		% "\n\n"

	-- update the class with the new fetters
	updateClass cid
	 $ cls { classFetters = fsCrushed }
		
	-- return a bool saying whether we made any progress with crushing
	--	fetters in this class.
	return $ or progresss

crushFetterC2 cid c
	= return False



-- | Try to crush a single parameter class constraint that is acting
--	on this node from the type graph.
crushFetterSingle
	:: Class			-- ^ node from graph
	-> Fetter			-- ^ the fetter to crush
	-> SquidM 
		( Class			-- the updated class
		, ( Bool		-- whether we made any progress
		  , Maybe Fetter))	-- perhaps a new fetter to replace the original with

crushFetterSingle 
	cls  
	f@(FConstraint vC [tC@(TClass k cidTarget)])
 = do	
	-- the cid of the node that the fetter was in
	let cidCls	= classId cls

	-- A single parameter type class constraint should always constrain
	--	the node that it's stored in.
	cidCls'		<- sinkClassId cidCls
 	cidTarget'	<- sinkClassId cidTarget
	
	when (cidCls' /= cidTarget')
	 $ panic stage
	 	$ "crushFetterSingle: Fetter in class " % cidCls' 
		% " constrains some other class " 	% cidTarget' % "\n"
		
	crushFetterSingle' k cidCls' cls f

crushFetterSingle' 
	k
	cid
	cls@Class 
		{ classType  		= Just tNode
		, classFetterSources 	= nodes } 
	  f@(FConstraint vC [tC@(TClass _ cidT)])

	-- crush a purity constraint
	| vC  == primPure
	= crushFetterPure cid cls f	

	-- try and crush some non-purity constraint
	| otherwise
	= do	
		-- crush some non-purify fetter
		mfsBits	<- crushFetterSingle_fromGraph k cid tNode vC

		-- the above call could return a number of simpler fetters
		case mfsBits of

		 -- we managed to crush it into something simpler
		 Just fsBits 
		  -> do	-- add all the smaller fetters back to the graph.
			let ?src	= TSI $ SICrushedF cid f
			trace	$ "    crushed fetters (fsBits)       = " % fsBits % "\n"
			
			progress	<- liftM or
					$  mapM addFetter fsBits
			
			Just cls'	<- lookupClass cid
						
			return	( cls'
				, (progress, Nothing))
		
		 -- we couldn't crush the fetter yet.
		 Nothing
		  -> 	return	( cls
				, (False, Just f))


	| otherwise
	= return (cls, (False, Nothing))


-- | Crush a purity constraint
crushFetterPure
	cid
	cls@Class 
		{ classType  		= Just tNode
		, classFetterSources 	= nodes } 
	  fPure@(FConstraint vC [tC@(TClass k cidT)])
 = do	
	trace	$ "    -- crushing purity constraint"	% "\n"

	-- flatten out the effect sum into individual atomic effects
	let effs_atomic = flattenTSum tNode
	trace	$ "    effs_atomic                    = " % effs_atomic % "\n"

	-- get the fetters of each atomic effect,
	mfsPurifiers		<- mapM purifyEffect_fromGraph effs_atomic
	trace	$ "    mfsPurifiers                   = " % mfsPurifiers % "\n"

	-- lookup the source of the original purity constraint, 
	--	and make the source info for constraints added due to purification.
	let Just fPureSrc	= lookup fPure nodes

	-- see if all the atomic effects could be purifier
	case sequence mfsPurifiers of
	 Just fsPurifiers	
	  -> crushFetterPure_success cid cls fPure fPureSrc fsPurifiers

	 Nothing		
	  -> crushFetterPure_failed cid cls fPure fPureSrc 
		(zip effs_atomic mfsPurifiers)

-- we have a new constraint that forces each of the atomic effects to be pure.
crushFetterPure_success cid 
	cls@Class 
		{ classType  		= Just tNode
		, classFetterSources 	= nodes } 
	fPure@(FConstraint vC [tC@(TClass k cidT)])
	fPureSrc
	fsPurifiers
 = do	
	-- lookup the source of the original purity constraint, 
	--	and make the source info for constraints added due to purification.
	let srcPurified		= TSI (SICrushedFS cid fPure fPureSrc)
	trace	$ "    srcPurified                    = " % srcPurified % "\n"

	-- add all the new fetters back to the graph
	--	the addition function returns True if the fetter added was a new constraint
	bsWasNewFetter		<- mapM (addFetterSource srcPurified) fsPurifiers
	trace	$ "    bsWasNewFetter                 = " % bsWasNewFetter % "\n"
		
	-- we've made progress on this class if any new fetters were added
	let madeProgress	= or bsWasNewFetter
	return (cls, (madeProgress, Just fPure))	


-- one of the effects could not be purified
crushFetterPure_failed cid cls fPure fPureSrc eff_mPurifiers
 = do	trace	$ ppr "-- purification failed"

	-- lookup the first effect that we couldn't make a purifier for
	let badAtomicEff : _
		= [ eff	| (eff, Nothing)	<- eff_mPurifiers ]

	-- lookup the type-source for the conflicting effect
	--	The nodes hold effect sums, so we need to look inside them
	--	to find which one holds our (single) conflicting effect
	let badAtomicEff_src : _ 	
		= [srcEff	| (eff,  srcEff)	<- classTypeSources cls
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
	return (cls, (False, Just fPure))


-- | Given an effect, produce either 
--	the fetter which purifies this effect
--	or an error saying why it cannot be purified.
--
purifyEffect_fromGraph
	:: Effect 
	-> SquidM (Maybe Fetter)

purifyEffect_fromGraph eff
	-- read
 	| TEffect v [tR@(TClass kR _)]	<- eff
	, v 	== primRead
	, kR	== kRegion
	= return $ Just $ FConstraint primConst [tR]

	-- head read
 	| TEffect v [tV@(TClass kV cidV)]	<- eff
	, v 	== primReadH
	, kV	== kValue
	= do	mHeadType	<- headTypeDownLeftSpine cidV
			
		case mHeadType of
		 Just tR@(TClass kR _)
		  | kR == kRegion	-> return $ Just $ FConstraint primConst [tR]
		 _			-> return $ Nothing
		
	-- deep read
	-- TODO: Do we realy want ReadT to be able to operate
	--	 on types of any kind?
 	| TEffect v [tR@(TClass kV _)]	<- eff
--	, kV	== kValue
	, v	== primReadT
	= return $ Just $ FConstraint primConstT [tR]
	
	-- effect variable
	| TClass kE cid			<- eff
	, kE	== kEffect
	= return $ Just $ FConstraint primPure [eff]

	| otherwise
	= return $ Nothing


-- | Crush a non-purity fetter that's constraining some node in the graph.
crushFetterSingle_fromGraph 
	:: Kind 
	-> ClassId			-- cid of class being constrained.
	-> Type				-- the node type being constrained
	-> Var				-- var of fetter ctor

	-> SquidM 			-- if Just [Fetters] then the original fetter is removed and these
					--			  new ones are added to the graph.
		(Maybe [Fetter])	--    Nothing        then leave the original fetter in the class.

crushFetterSingle_fromGraph k cid tNode vC
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
		 TBot k
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
		 TBot k
		  | k == kRegion	-> return $ Just [ FConstraint primMutable [TClass k cid] ]
		  | k == kClosure	-> return $ Just []
		  | k == kEffect	-> return $ Just []
		  | otherwise		-> return   Nothing
		
		 _ 	-> return Nothing
	

	| otherwise
	= return Nothing



