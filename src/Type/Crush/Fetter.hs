
module Type.Crush.Fetter
	( crushFetterC )

where

import Type.Feed
import Type.Trace
import Type.State
import Type.Util
import Type.Class
import Type.Exp

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
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Crush.Fetter"

crushFetterC :: ClassId -> SquidM ()
crushFetterC cid
 = do
	-- Lookup the class for the fetter
	Just fetterC		<- lookupClass cid
	let (FConstraint v ts)	= classFetter fetterC

	-- Load up the arguments for this fetter
	let argCids		= map (\(TClass k cid) -> cid) ts

	argTs_traced		<- mapM traceType argCids
	let argTs_packed	= map packType argTs_traced

	let fetter		= FConstraint v argTs_packed

	-- Try and reduce it
	let fetters_reduced	= reduceFetter fetter

 	trace	$ "\n"
		% "*   crushFetterC " 		% cid			% "\n"
		% "    fetter           = " 	% fetter		% "\n"
		% "    fetter_reduced   = "	% fetters_reduced	% "\n"

	-- Update the graph
	case fetters_reduced of

	 -- we didn't manage to crush it
	 Nothing
	  -> return ()
	  
	 -- crushed this fetter into smaller ones
	 Just fs
	  -> do	delClass cid
	  	let ?src = TSCrushed fetter
	  	mapM (feedFetter Nothing) fs
		
		unregisterClass (Var.bind v) cid

		return ()

	return ()


reduceFetter :: Fetter -> Maybe [Fetter]
reduceFetter f@(FConstraint v [t])
	-- lazy head region	    
	| Var.FLazyH	<- Var.bind v
	, Just tR	<- slurpHeadR t
	= Just [FConstraint primLazy [tR]]
	
	| otherwise 
	= Nothing

reduceFetter _
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


