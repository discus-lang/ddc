
module Type.Crush.Fetter
	( crushFetterC 
	, crushFetter)

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

import Shared.Error
import Shared.VarPrim
import Shared.Var		(VarBind, NameSpace(..))
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)


-----
debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Crush.Fetter"

crushFetterC 
	:: ClassId 
	-> SquidM Bool	-- whether we crushed something from this class

crushFetterC cid
 = do	Just c	<- lookupClass cid
 	crushFetterC2 cid c

crushFetterC2 cid (ClassForward cid')
	= crushFetterC cid'

crushFetterC2 cid (ClassFetter { classFetter = f })
	= crushFetterMulti cid f

crushFetterC2 cid 
	c@(Class	
		{ classKind	= k
		, classType	= Just tNode
		, classFetters 	= fs })
 = do	
	trace	$ "*   crushFetterC "	% k % cid		% "\n"
		% "    tNode      = " % tNode			% "\n"
		% "    fs         = " % fs			% "\n"
		
	(progresss, mfsReduced)	
		<- liftM unzip
		$  mapM (crushFetterSingle cid c tNode) fs

	trace	$ "    progress   = " % progresss		% "\n\n"

	-- update the class with the new fetters
	let fsReduced	= catMaybes mfsReduced
	modifyClass cid
	 $ \c -> c { classFetters = map TFetter $ fsReduced }
		
	return $ or progresss

crushFetterC2 cid c
	= return False

-- crushFetterSingle -------------------------------------------------------------------------------
crushFetterSingle cid c tNode (TFetter f@(FConstraint vC [TClass k cidC]))
 = do	
	cid'	<- sinkClassId cid
 	cidC'	<- sinkClassId cidC
	
	when (cid' /= cidC')
	 $ panic stage
	 	$ "crushFetterSingle: Fetter in class " % cid % " constrains some other class " % cidC' % "\n"
		
	crushFetterSingle' cid c tNode vC cidC f

crushFetterSingle' cid c@Class { classNodes = nodes} tNode vC cidC f

	-- keep the original fetter when crushing purity
	| vC	== primPure
	= do	fEff	<- crushPure c cidC f tNode
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
			
	-- could crush this fetter
	| Just fsBits	<- crushFetter $ FConstraint vC [tNode]
	= do
		trace	$ "    fsBits     = " % fsBits			% "\n"
		
		let ?src	= TSI $ SICrushedF cid f
		progress	<- liftM or
				$ mapM addFetter fsBits
						
		return	( progress
			, Nothing)
	-- can't crush
	| otherwise
	= 	return	( False
			, Just f)
			

-- Crush a purity fetter.
--	This can generate an error if the effect that the fetter is acting 
--	on cannot be purified.
--
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
	let mFsPureSrc	
		= map (purifyEffSrc c fPure cidEff) 
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


-- | Crush a non-purity fetter
--
crushFetter :: Fetter -> Maybe [Fetter]
crushFetter (FConstraint vC ts)
	-- lazy head
	| vC	== primLazyH
	, [t]		<- ts
	, Just tR	<- slurpHeadR t
	= Just [FConstraint primLazy [tR]]
	
	-- deep mutability
	| vC	 == primMutableT
	, [t]		<- ts
	, TData{}	<- t
	= let	(rs, ds)	= slurpVarsRD t
		fsRegion	= map (\r -> FConstraint primMutable  [r]) rs
		fsData		= map (\d -> FConstraint primMutableT [d]) ds
	  in	Just $ fsRegion ++ fsData
	  
	-- deep const
	| vC	== primConstT
	, [t]		<- ts
	, TData{}	<- t
	= let 	(rs, ds)	= slurpVarsRD t
		fsRegion	= map (\r -> FConstraint primConst  [r]) rs
		fsData		= map (\d -> FConstraint primConstT [d]) ds
	  in	Just $ fsRegion ++ fsData
	  
	| otherwise
	= Nothing


-- | Try and purify this effect, 
--	tagging the new constraint with the appropriate typesource
purifyEffSrc :: Class -> Fetter -> ClassId -> Effect -> Maybe (Fetter, TypeSource)

purifyEffSrc 
	cPure@Class { classNodes = nodes }
	fPure cidEff 
	eff@(TClass KEffect cidE)

 = let	Just src	= lookup (TFetter fPure) nodes
   in	Just 	( FConstraint primPure [eff]
		, TSI (SICrushedFS cidEff fPure src) )

purifyEffSrc
	cPure fPure cidEff
	eff
 = case purifyEff eff of
	Nothing		-> Nothing
	Just fNew	-> Just (fNew, TSI (SIPurify cidEff eff))
	

-- | produce either 
--	the fetter which purifies this effect
--	or an error saying why it cannot be purified.
--
purifyEff :: Type -> Maybe Fetter
purifyEff eff
	-- read
 	| TEffect v [tR@(TClass KRegion _)]	<- eff
	, v == primRead
	= Just $ FConstraint primConst [tR]

	-- deep read
 	| TEffect v [tR@(TClass KData _)]	<- eff
	, v == primReadT
	= Just $ FConstraint primConstT [tR]
	
	-- effect variable
	| TClass KEffect cid			<- eff
	= Just $ FConstraint primPure [eff]

	| otherwise
	= Nothing
	

-- | Slurp the head region from this type, if there is one.
slurpHeadR :: Type -> Maybe Type
slurpHeadR tt
 = case tt of
 	TFetters fs t	
	 -> slurpHeadR t

	TData v (tR@(TClass KRegion _) : _)
	 -> Just tR
	 
	_ -> Nothing


-- crushFetterMulti --------------------------------------------------------------------------------

-- projections are handled by Type.Crush.Proj instead
crushFetterMulti cid (FProj{})
	= return False

crushFetterMulti cid (FConstraint vC ts)
 = do
{-	-- Load up the arguments for this fetter
	let argCids		= map (\(TClass k cid) -> cid) ts
	argTs_traced		<- mapM traceType argCids
	let argTs_packed	= map packType argTs_traced

	-- this is the fetter with its args traced
	let fetter		= FConstraint vC argTs_packed

	-- Try and reduce it
	let fetters_reduced	= Nothing -- fetter --  reduceFetter cid fetter

 	trace	$ "\n"
		% "*   crushFetterC " 		% cid			% "\n"
		% "    fetter           = " 	% fetter		% "\n"
--		% "    fetter_reduced   = "	% fetters_reduced	% "\n"

	-- Update the graph
	case fetters_reduced of

	 -- we didn't manage to crush it
	 Nothing
	  -> return False
	  
	 -- crushed this fetter into smaller ones
	 Just fs
	  -> do	
	  	-- delete and unregister the old fetter class
	  	delClass cid
		unregisterClass (Var.bind vC) cid

		-- add the new, crushed pieces
	  	let ?src = TSCrushed fetter
	  	mapM (feedFetter Nothing) fs
		return True
-}
	return False


