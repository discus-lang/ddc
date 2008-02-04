
module Type.Crush.Fetter
	( crushFetterC 
	, crushFetter)

where

import Type.Feed
import Type.Trace
import Type.State
import Type.Util
import Type.Class
import Type.Location
import Type.Exp
import Type.Dump

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
	(Class	{ classKind	= k
		, classType	= Just tNode
		, classFetters 	= fs })
 = do	
	trace	$ "*   crushFetterC "	% k % cid		% "\n"
		% "    tNode      = " % tNode			% "\n"
		% "    fs         = " % fs			% "\n"
		
	(progresss, mfsReduced)	
		<- liftM unzip
		$  mapM (crushFetterSingle cid tNode) fs

	trace	$ "    progress   = " % progresss		% "\n\n"

	-- update the class with the new fetters
	let fsReduced	= catMaybes mfsReduced
	modifyClass cid
	 $ \c -> c { classFetters = map TFetter $ fsReduced }
		
	return $ or progresss

crushFetterC2 cid c
	= return False

-- crushFetterSingle -------------------------------------------------------------------------------
crushFetterSingle cid tNode (TFetter f@(FConstraint vC [TClass k cidC]))
 = do	
	cid'	<- sinkClassId cid
 	cidC'	<- sinkClassId cidC
	
	when (cid' /= cidC')
	 $ panic stage
	 	$ "crushFetterSingle: Fetter in class " % cid % " constrains some other class " % cidC' % "\n"
		
	crushFetterSingle' cid tNode vC f

crushFetterSingle' cid tNode vC f

	-- keep the original fetter when crushing purity
	| vC	== primPure
	, Just fsBits	<- crushFetter $ FConstraint vC [tNode]
	= do	
		trace	$ "    fsBits     = " % fsBits			% "\n"

		let ?src	= TSI $ SICrushed f
		progress	<- liftM or 
				$  mapM addFetter fsBits

		return	( progress
			, Just f)
			
	-- could crush this fetter
	| Just fsBits	<- crushFetter $ FConstraint vC [tNode]
	= do
		trace	$ "    fsBits     = " % fsBits			% "\n"

		let ?src	= TSI $ SICrushed f
		progress	<- liftM or
				$ mapM addFetter fsBits
						
		return	( progress
			, Nothing)
	-- can't crush
	| otherwise
	= 	return	( False
			, Just f)
			

crushFetter :: Fetter -> Maybe [Fetter]
crushFetter (FConstraint vC ts)
	-- purity
	| vC	== primPure
	, [t]		<- ts
	= Just $ catMaybes $ map purifyEff $ flattenTSum t
	
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
	

-- | produce the fetter which purifies this effect
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
	= panic stage
		$ "purifyEff: can't purify " % eff % "\n"	
	

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


