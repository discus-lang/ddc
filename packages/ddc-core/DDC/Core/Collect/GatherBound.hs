
module DDC.Core.Collect.GatherBound
        (GatherBound(..))
where
import DDC.Core.Exp
import DDC.Type.Collect.GatherBound
import qualified Data.Set               as Set


instance GatherBound n (Exp a p n) where
 gatherBound xx
  = case xx of
        XPrim{}         -> Set.empty
        XVar _ u        -> gatherBound u
        XCon _ u        -> gatherBound u
        XApp _ x1 x2    -> Set.unions [gatherBound x1, gatherBound x2]
        XLam _ b  x     -> Set.unions [gatherBound b,  gatherBound x]
        XLet{}          -> error "exp free not done yet"
        XCase{}         -> error "exp free not done yet"
        XCast _ x c     -> Set.unions [gatherBound x,  gatherBound c]
        XType t         -> gatherBound t
        XWitness w      -> gatherBound w


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
