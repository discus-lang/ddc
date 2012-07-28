module DDC.Core.Transform.Rewrite.Disjoint
    ( checkDisjoint )
where

import DDC.Core.Exp

import qualified DDC.Type.Compounds	as T
--import qualified DDC.Type.Exp		as T
import qualified DDC.Type.Sum		as TS

-- TODO crush, then do a proper check
checkDisjoint
    :: (Eq n, Ord n, Show n)
    => Type n				-- ^ target, might be "Disjoint f g"
    -> (Bound n -> Bound n -> Bool)	-- ^ distinctness map.
    -> Bool
checkDisjoint c _knownDistinct
 | [TCon (TyConWitness TwConDisjoint), TSum f, TSum g]
		    <- T.takeTApps c
 , fEffs	    <- TS.toList   f
 , gEffs	    <- TS.toList   g
 , fCons	    <- map (head . T.takeTApps) fEffs
 , gCons	    <- map (head . T.takeTApps) gEffs
 -- do a quick check - if they're all harmless, we don't care
 , all harmless (fCons++gCons)
 = True
 | otherwise
 = False
 where
    harmless (TCon (TyConSpec con)) = harmlessCon con
    harmless _ = False
    harmlessCon m = m `elem`
	[ TcConRead, TcConHeadRead, TcConDeepRead
	, TcConAlloc, TcConDeepAlloc]

