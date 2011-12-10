
module DDC.Core.Collect.Free
        (Free(..))
where
import DDC.Core.Exp
import DDC.Type.Collect.Free
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


instance Free n (Exp a n) where
 free env xx
  = case xx of
        XVar _ u        -> free env u
        XCon _ u        -> free env u
        XApp _ x1 x2    -> Set.unions [free env x1, free env x2]
        XLam _ b  x     -> Set.unions [free env b,  free (Env.extend b env) x]
        XLet{}          -> error "exp free not done yet"
        XCase{}         -> error "exp free not done yet"
        XCast _ x c     -> Set.unions [free env x,  free env x]
        XType t         -> free env t
        XWitness w      -> free env w


instance Free n (Cast n) where
 free env cc
  = case cc of
        CastWeakenEffect eff    -> free env eff
        CastWeakenClosure clo   -> free env clo
        CastPurify w            -> free env w
        CastForget w            -> free env w


instance Free n (Witness n) where
 free env ww
  = case ww of
        WCon{}          -> Set.empty
        WVar u          -> free env u
        WApp  w1 w2     -> Set.unions [free env w1, free env w2]
        WJoin w1 w2     -> Set.unions [free env w1, free env w2]

