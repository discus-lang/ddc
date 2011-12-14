
-- | Collect the free variables in a core expression.
module DDC.Core.Collect.Free
        (Free(..))
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Collect.Free
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


instance Free n (Exp a n) where
 free env xx
  = case xx of
        XVar _ u        -> free env u
        XCon _ u        -> free env u
        XApp _ x1 x2    -> Set.unions [free env x1,  free env x2]
        XLam _ b  x     -> Set.unions [free env b,   free (Env.extend b env) x]
        XLet _ lts x    -> Set.unions [free env lts, free (Env.extends (bindsOfLets lts) env) x]

        XCase{}         -> error "free[Exp] XCase"

        XCast _ x c     -> Set.unions [free env x,  free env c]
        XType t         -> free env t
        XWitness w      -> free env w


instance Free n (Lets a n) where
 free env lts 
  = case lts of
        LLet b x        -> Set.unions [free env b,   free (Env.extend b env) x]

        LRec bxs        
         -> let (bs, xs) = unzip bxs
                env'     = Env.extends (map fst bxs) env
            in  Set.unions 
                        [ Set.unions $ map (free env)  bs
                        , Set.unions $ map (free env') xs ]
        
        LLetRegion b bs
         -> let env1    = Env.extend b env
            in  Set.unions 
                        [ free env b
                        , Set.unions $ map (free env1) bs]

        LWithRegion b   -> free env b
        

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

