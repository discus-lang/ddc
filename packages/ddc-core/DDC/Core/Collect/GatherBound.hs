{-# OPTIONS_HADDOCK hide #-}
-- | Gather all bound names in a thing,
--   independent of whether they are locally bound or not.
module DDC.Core.Collect.GatherBound
        (GatherBound(..))
where
import DDC.Core.Exp
import DDC.Type.Collect.GatherBound
import qualified Data.Set               as Set


instance GatherBound n (Exp a n) where
 gatherBound xx
  = case xx of
        XVar  _ u       -> gatherBound u
        XCon  _ u       -> gatherBound u
        XApp  _ x1 x2   -> Set.unions [gatherBound x1, gatherBound x2]
        XLAM  _ b  x    -> Set.unions [gatherBound b,  gatherBound x]
        XLam  _ b  x    -> Set.unions [gatherBound b,  gatherBound x]
        XLet  _ lt x    -> Set.unions [gatherBound lt, gatherBound x]
        XCase _ x alts  -> Set.unions [gatherBound x,  Set.unions $ map gatherBound alts]
        XCast _ x c     -> Set.unions [gatherBound x,  gatherBound c]
        XType t         -> gatherBound t
        XWitness w      -> gatherBound w


instance GatherBound n (Pat n) where
 gatherBound pp
  = case pp of
        PDefault        -> Set.empty
        PData u bs      -> Set.unions (gatherBound u : map gatherBound bs)


instance GatherBound n (Alt a n) where
 gatherBound (AAlt p x)
        = Set.unions [gatherBound p, gatherBound x]

instance GatherBound n (Lets a n) where
 gatherBound xx
  = case xx of
        LLet _ b x
         -> Set.unions [gatherBound b, gatherBound x]
 
        LRec bxs        
         -> Set.unions
                [ Set.unions $ map (gatherBound . fst) bxs
                , Set.unions $ map (gatherBound . snd) bxs]

        LLetRegion b bs
         -> Set.unions
                $ gatherBound b
                : map gatherBound bs

        LWithRegion b   -> gatherBound b


instance GatherBound n (Cast n) where
 gatherBound cc
  = case cc of
        CastWeakenEffect eff    -> gatherBound eff
        CastWeakenClosure clo   -> gatherBound clo
        CastPurify w            -> gatherBound w
        CastForget w            -> gatherBound w


instance GatherBound n (Witness n) where
 gatherBound ww
  = case ww of
        WCon{}          -> Set.empty
        WVar u          -> gatherBound u
        WApp  w1 w2     -> Set.unions [gatherBound w1, gatherBound w2]
        WJoin w1 w2     -> Set.unions [gatherBound w1, gatherBound w2]
        WType t         -> gatherBound t

