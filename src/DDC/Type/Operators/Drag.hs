{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Type.Operators.Drag
	(dragT)
where
import DDC.Type.Collect.FreeTVars
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Type.Transform
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

{-
import Debug.Trace
import DDC.Type.Pretty		()
import DDC.Main.Pretty
-}

-- For a type like the following.
-- @
--  *1 	:- *2 =  *3 !4
--	,  *3 =  *4 *5
--      ,  !4 :> !6 + !10
--      ,  !6 :> !7 + !8 + !9
-- @
--
-- As !6 only appears in other effect constraints, we can drag the right 
-- into its occurrences to give:
--
--  *1 	:- *2 =  *3 !4
--	,  *3 =  *4 *5
--      ,  !4 :> !6 + !7 + !8 + !9 + !10
-- 
dragT :: Set Type -> Type -> Type
dragT cidsNoDrag tt
 = case tt of
	TConstrain t crs
	 -> let	cidsNoDrag'	= Set.union cidsNoDrag (freeTClasses t)
	    in	TConstrain t (dragCrs cidsNoDrag' crs)

	_ -> tt
	

dragCrs :: Set Type -> Constraints -> Constraints
dragCrs tsNoDrag (Constraints crsEq crsMore crsOther)
 = let	
	-- Can't drag any constraints that appear on the right of eq constraints.
	tsEqRight	= Set.unions 
			$ map freeTClasses
			$ Map.elems crsEq

	-- Partition the :> constraints into the ones we're going to keep vs the ones
	-- we're going to inline.
	tsLeave	= Set.union tsNoDrag tsEqRight
	(crsLeave, crsSub)
			= Map.partitionWithKey (\k _ -> Set.member k tsLeave)
			$ crsMore

	-- Inline constraints into the ones we're keeping.
	crsLeave'	= Map.map (dragMore tsNoDrag crsSub) crsLeave
				
   in{-	trace (pprStrPlain 
		$ vcat	[ "tsSub   = " % crsSub
			, "crsMore = " % crsLeave'])
	 $-} Constraints crsEq crsLeave' crsOther
	

dragMore :: Set Type -> Map Type Type -> Type -> Type
dragMore tsNoDrag tsSub t2
	= transformT (dragVar tsNoDrag tsSub) t2
	
dragVar tsNoDrag tsSub tt
 = case tt of
	TVar k _
	  |  not $ Set.member tt tsNoDrag
 	  ,  Just tt'	<- Map.lookup tt tsSub
 	  -> makeTSum k 
		[ tt
		, dragMore (Set.insert tt tsNoDrag) tsSub tt']

	_ -> tt

