{-# OPTIONS -fno-warn-incomplete-record-updates #-}

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
debug	= False
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
	trace	$ "*   crushFetterC "   % k % cid		% "\n"
		% "    tNode        = " % tNode			% "\n"
		% "    fs           = " % fs			% "\n"

	-- try to crush each fetter in turn, into smaller bits.
	--	crushing a particular fetter (like Pure) might also cause
	--	the type in the node to change (such as by removing an effect term)
	(cls', progressFetters)	
		<- mapAccumLM crushFetterSingle cls
		$  fs
	
	let (progresss, mfsCrushed)
			= unzip progressFetters

	let fsCrushed	= catMaybes mfsCrushed
	
	trace	$ "    progress     = " % progresss		% "\n"
		% "    tNode'       = " % classType cls'	% "\n"
		% "    fsCrushed    = " % fsCrushed		% "\n\n"

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
		
	crushFetterSingle' cidCls' cls f

crushFetterSingle' 
	cid
	cls@Class 
		{ classType  		= Just tNode
		, classFetterSources 	= nodes } 
	  f@(FConstraint vC [tC@(TClass k cidT)])

	-- crush a purity constraint
	| vC  == primPure
	= do	trace	$ "    - crush pure"	% "\n"

		-- flatten out the effect sum into individual atomic effects
		let effs_atomic = flattenTSum tNode
		trace	$ "    effs_atomic  = " % effs_atomic % "\n"

		-- get the fetters of each atomic effect,
		--	TODO: handle purification errors
		mfsPurifiers		<- mapM purifyEffect_fromGraph effs_atomic
		trace	$ "    mfsPurifiers = " % mfsPurifiers % "\n"
		let Just fsPurifiers	= sequence mfsPurifiers	

		-- lookup the source of the original purity constraint, 
		--	and make the source info for constraints added due to purification.
		let Just src		= lookup f nodes
		let srcPurified		= TSI (SICrushedFS cid f src)
		trace	$ "    srcPurified  = " % srcPurified % "\n"

		-- add all the new fetters back to the graph
		--	the addition function returns True if the fetter added was a new constraint
		bsWasNewFetter		<- mapM (addFetterSource srcPurified) fsPurifiers
		trace	$ "    bsWasNewFetter = " % bsWasNewFetter % "\n"
		
		-- we've made progress on this class if any new fetters were added
		let madeProgress	= or bsWasNewFetter
		return (cls, (madeProgress, Just f))


	-- try and crush some non-purity constraint
	| otherwise
	= do	
		-- crush some non-purify fetter
		mfsBits	<- crushFetterSingle_fromGraph vC cid

		-- the above call could return a number of simpler fetters
		case mfsBits of

		 -- we managed to crush it into something simpler
		 Just fsBits 
		  -> do	-- add all the smaller fetters back to the graph.
			let ?src	= TSI $ SICrushedF cid f
			progress	<- liftM or
					$ mapM addFetter fsBits
			
			Just cls'	<- lookupClass cid
						
			return	( cls'
				, (progress, Nothing))
		
		 -- we couldn't crush the fetter yet.
		 Nothing
		  -> 	return	( cls
				, (False, Just f))


	| otherwise
	= return (cls, (False, Nothing))



{-
	-- keep the original fetter when crushing purity
	| vC	== primPure
	= return (False, Nothing)
	= do	fEff	<- crushPure c cid f tNode
		case fEff of

			-- fetter crushed ok
			Left fsBitsSrc
			 -> do	trace	$ "    fsBits     = " % fsBitsSrc		% "\n"
		
				-- record the source of the new fetter based on the old one
				progress	<- liftM or 
						$  mapM (\(f, src) -> addFetterSource src f) fsBitsSrc

				return	( progress
					, Just f)
			
			-- got a purity error
			Right err
			 -> do	trace	$ ppr "    ! purity error\n"
			 
			 	addErrors [err]
				return 	( False
					, Just f)
			
-}			

-- Crush a purity fetter.
--	This can generate an error if the effect that the fetter is acting 
--	on cannot be purified.
--
{-
crushPure 
	:: Class 	-- ^ The class of the purify effect (for error reporting)
	-> ClassId	-- ^ The classId of the effect being purified (for error reporting)
	-> Fetter 	-- ^ The purity fetter being crushed (for error reporting)
	-> Effect	-- ^ The effect to purify

	-> SquidM (Either [(Fetter, TypeSource)] Error)

crushPure c cidEff fPure tNode
 = do
	-- flatten out the sum into individual effects
	let effs = flattenTSum tNode
	trace	$ "    effs_flat  = " % effs % "\n"

	-- try and generate the additional constraints needed to purify 
	--	each effect.
	mFsPureSrc	
		<- mapM (purifyEffSrc c fPure cidEff) 
		$ flattenTSum tNode

	-- See if any of the effects couldn't be purified
	let effsBad	
		= catMaybes
		$ map (\(eff, mfPureSrc) 
			-> case mfPureSrc of
				Nothing	-> Just eff
				Just _	-> Nothing)
		$ zip effs mFsPureSrc
								
	trace	$ "    effs_bad   = " % effsBad % "\n"

	return 	$ Left $ catMaybes mFsPureSrc

 	case effsBad of
	  	[]		-> return $ Left  $ catMaybes mFsPureSrc
		(effBad : _)	
		 -> do	eff	<- makePurityError c fPure effBad
		 	return	$ Right eff
		


-- | Make an error for when the purity fetter in this class could not be satisfied.
makePurityError :: Class -> Fetter -> Effect -> SquidM Error
makePurityError 
	c@Class	{ classNodes = nodes }
	fPure@(FConstraint vC [TClass _ cidE])
	effBad
 = do
	-- lookup the type-source for the purify fetter
-- 	fSrc : _	= [tsPure	| (TFetter (FConstraint v _), tsPure)	<- nodes
--					, v == primPure ]

	Just fSrc	<- traceFetterSource vC cidE

	-- lookup the type-source for the conflicting effect
	--	The nodes hold effect sums, so we need to look inside them
	--	to find which one holds our (single) conflicting effect
	let effSrc : _ 	= [tsEff	| (eff,  tsEff)		<- nodes
					, elem effBad $ flattenTSum eff]
					
	return 	$ ErrorCannotPurify
	   		{ eEffect	= effBad
			, eEffectSource	= effSrc
			, eFetter	= fPure
			, eFetterSource	= fSrc }


-- | Try and purify this effect, 
--	tagging the new constraint with the appropriate typesource
purifyEffSrc 
	:: Class 
	-> Fetter 
	-> ClassId 
	-> Effect 
	-> SquidM (Maybe (Fetter, TypeSource))

purifyEffSrc 
	cPure@Class { classNodes = nodes }
	fPure cidEff 
	eff@(TClass kE cidE)
	 
 | kE	== kEffect
 = do	let (Just src)	= lookup (TFetter fPure) nodes
   	return	$ Just 	( FConstraint primPure [eff]
			, TSI (SICrushedFS cidEff fPure src) )

purifyEffSrc
	cPure fPure cidEff
	eff
 = do
	mfNew	<- purifyEff eff
	case mfNew of
	 Nothing	-> return $ Nothing
	 Just fNew	-> return $ Just (fNew, TSI (SIPurify cidEff eff))
	




-}

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
	:: Var				-- var of fetter ctor
	-> ClassId			-- cid of class being constrained.
	-> SquidM (Maybe [Fetter])

crushFetterSingle_fromGraph vFetter cid
	-- lazy head
	| vFetter	== primLazyH
	= do	mtHead	<- headTypeDownLeftSpine cid
		case mtHead of
			Just t	-> return $ Just [FConstraint primLazy [t]]
			_	-> return Nothing


	| otherwise
	= return Nothing



