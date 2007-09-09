
module Type.Crush.Fetter
--	( crushFetterC )
	()

where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim
import Shared.Var		(VarBind, NameSpace(..))

import Type.Exp
import Type.Util
import Type.State
import Type.Class
import Type.Feed

import Type.Crush.Unify

-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Squid.CrushFetter"

{-
crushFetterC :: ClassId -> SquidM ()
crushFetterC cid
 = do 	Just c		<- lookupClass cid

	let [TFetter f@(FClass vFetter _)] 
		=  classQueue c

	-- Try and crush this fetter.
	mfsCrushed	<- crushFetter f

	case mfsCrushed of
	 -- The fetter didn't reduce.
	 Nothing	-> return ()

	 -- The fetter reduced into a simpler form.
	 Just fsCrushed
	  -> do
		-- Add the new fetters to the graph.
	  	let ?typeSource	= TSNil
		mapM_ feedFetter fsCrushed
		
		-- Update the old class and unregister the fetter.
		let c'	= c { classQueue = [] }
		updateClass cid c' 
		
		unregisterClass (Var.bind vFetter) cid
				
		return ()


crushFetter :: Fetter -> SquidM (Maybe [Fetter])
crushFetter f
 = do 	fRefresh	<- refresh f
	let mfsNew 	= reduceFetter fRefresh
	
	trace	$ "*   Crush.CrushFetter.crushFetter\n"
		% "    f        = " % f 	% "\n"
		% "    fRefresh = " % fRefresh 	% "\n"
		% "    mfsNew   = " % mfsNew    % "\n"
		% "\n"
		
	return mfsNew


reduceFetter :: Fetter -> Maybe [Fetter]
reduceFetter f@(FClass v [t@(TCon vCon ts)])
	| Var.FConstT		<- Var.bind v
	= let	bits		= catMap reduceDataRT ts
		fsRegion	= [FClass primConst    [r] | r@(TRegion{})	<- bits]
		fsType		= [FClass primConstT   [t] | t@(TClass{})	<- bits]
	  in	Just (fsRegion ++ fsType)

	| Var.FMutableT		<- Var.bind v
	= let	bits		= catMap reduceDataRT ts
	 	fsRegion	= [FClass primMutable  [r] | r@(TRegion{})	<- bits]
		fsType		= [FClass primMutableT [t] | t@(TClass{})	<- bits]
		
          in	Just (fsRegion ++ fsType)
	    
	| Var.FLazyH		<- Var.bind v
	, (tR@(TRegion r) : tsRest)	<- ts
	= 	Just [FClass primLazy [tR]]
	
	| otherwise 
	= Nothing
	
reduceFetter f
	= Nothing
	
-}




