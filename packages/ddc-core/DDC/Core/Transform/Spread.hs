
-- | Spreading of type annotations from binders and the environment into bound
--   variables and constructors.
module DDC.Core.Transform.Spread
        (Spread(..))
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Transform.Spread
import qualified DDC.Type.Env           as Env


instance Spread (Exp a) where
 spread env xx
  = case xx of
        XVar a u        -> XVar a (spread env u)
        XCon a u        -> XCon a (spread env u)
        XApp a x1 x2    -> XApp a (spread env x1) (spread env x2)

        XLam a b x      
         -> let b'      = spread env b
            in  XLam a b' (spread (Env.extend b' env) x)
            
        XLet a lts x
         -> let lts'    = spread env lts
                env'    = Env.extends (bindsOfLets lts') env
            in  XLet a lts' (spread env' x)
         
        XCase a x alts
         -> let x'      = spread env x
                alts'   = map (spread env) alts
            in  XCase a x' alts'

        XCast a c x     -> XCast a (spread env c) (spread env x)
        
        XType t         -> XType    (spread env t)
        XWitness w      -> XWitness (spread env w)


instance Spread Cast where
 spread env cc
  = case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  (spread env eff)
        CastWeakenClosure clo   -> CastWeakenClosure (spread env clo)
        CastPurify w            -> CastPurify        (spread env w)
        CastForget w            -> CastForget        (spread env w)


instance Spread Pat where
 spread env pat
  = case pat of
        PDefault        -> PDefault
        PData u bs      -> PData (spread env u) (map (spread env) bs)


instance Spread (Alt a) where
 spread env alt
  = case alt of
        AAlt p x
         -> let p'      = spread env p
                env'    = Env.extends (bindsOfPat p') env
            in  AAlt p' (spread env' x)


instance Spread (Lets a) where
 spread env lts
  = case lts of
        LLet m b x     
         -> let m'      = spread env m
                b'      = spread env b
            in  LLet m' b' (spread (Env.extend b' env) x)
        
        LRec bxs
         -> let (bs, xs) = unzip bxs
                bs'      = map (spread env) bs
                env'     = Env.extends bs' env
                xs'      = map (spread env') xs
             in LRec (zip bs' xs')

        LLetRegion b bs
         -> let b'      = spread env b
                env'    = Env.extend b' env
                bs'     = map (spread env') bs
            in  LLetRegion b' bs'

        LWithRegion b
         -> LWithRegion (spread env b)


instance Spread LetMode where
 spread env lm
  = case lm of
        LetStrict        -> LetStrict
        LetLazy Nothing  -> LetLazy Nothing
        LetLazy (Just w) -> LetLazy (Just $ spread env w)


instance Spread Witness where
 spread env ww
  = case ww of
        WCon  wicon     -> WCon wicon
        WVar  u         -> WVar  (spread env u)
        WApp  w1 w2     -> WApp  (spread env w1) (spread env w2)
        WJoin w1 w2     -> WJoin (spread env w1) (spread env w2)
        WType t1        -> WType (spread env t1)
