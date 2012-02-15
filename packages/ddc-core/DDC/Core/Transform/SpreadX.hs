
-- | Spread type annotations from binders and the environment into bound
--   occurrences of variables and constructors.
module DDC.Core.Transform.SpreadX
        (SpreadX(..))
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Transform.SpreadT
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env


class SpreadX (c :: * -> *) where

 -- | Spread type annotations from binders and the environment into bound
 --   occurrences of variables and constructors.
 --
 --   Also convert `Bound`s to `UPrim` form if the environment says that
 --   they are primitive.
 spreadX :: forall n. Ord n
         => Env n -> Env n -> c n -> c n


instance SpreadX (Exp a) where
 spreadX kenv tenv xx 
  = let down = spreadX kenv tenv 
    in case xx of
        XVar a u        -> XVar a (down u)
        XCon a u        -> XCon a (down u)
        XApp a x1 x2    -> XApp a (down x1) (down x2)

        XLAM a b x
         -> let b'      = spreadT kenv b
            in  XLAM a b' (spreadX (Env.extend b' kenv) tenv x)

        XLam a b x      
         -> let b'      = down b
            in  XLam a b' (spreadX kenv (Env.extend b' tenv) x)
            
        XLet a lts x
         -> let lts'    = down lts
                kenv'   = Env.extends (specBindsOfLets   lts') kenv
                tenv'   = Env.extends (valwitBindsOfLets lts') tenv
            in  XLet a lts' (spreadX kenv' tenv' x)
         
        XCase a x alts  -> XCase a  (down x) (map down alts)
        XCast a c x     -> XCast a  (down c) (down x)
        XType t         -> XType    (spreadT kenv t)
        XWitness w      -> XWitness (down w)


instance SpreadX Cast where
 spreadX kenv tenv cc
  = let down = spreadX kenv tenv 
    in case cc of
        CastWeakenEffect eff  -> CastWeakenEffect  (spreadT kenv eff)
        CastWeakenClosure clo -> CastWeakenClosure (spreadT kenv clo)
        CastPurify w          -> CastPurify        (down w)
        CastForget w          -> CastForget        (down w)


instance SpreadX Pat where
 spreadX kenv tenv pat
  = let down    = spreadX kenv tenv
    in case pat of
        PDefault        -> PDefault
        PData u bs      -> PData (down u) (map down bs)


instance SpreadX (Alt a) where
 spreadX kenv tenv alt
  = case alt of
        AAlt p x
         -> let p'       = spreadX kenv tenv p
                tenv'    = Env.extends (bindsOfPat p') tenv
            in  AAlt p' (spreadX kenv tenv' x)


instance SpreadX (Lets a) where
 spreadX kenv tenv lts
  = let down = spreadX kenv tenv
    in case lts of
        LLet m b x       -> LLet (down m) (down b) (down x)
        
        LRec bxs
         -> let (bs, xs) = unzip bxs
                bs'      = map (spreadX kenv tenv) bs
                tenv'    = Env.extends bs' tenv
                xs'      = map (spreadX kenv tenv') xs
             in LRec (zip bs' xs')

        LLetRegion b bs
         -> let b'       = spreadT kenv b
                kenv'    = Env.extend b' kenv
                bs'      = map (spreadX kenv' tenv) bs
            in  LLetRegion b' bs'

        LWithRegion b
         -> LWithRegion (spreadX kenv tenv b)


instance SpreadX LetMode where
 spreadX kenv tenv lm
  = case lm of
        LetStrict        -> LetStrict
        LetLazy Nothing  -> LetLazy Nothing
        LetLazy (Just w) -> LetLazy (Just $ spreadX kenv tenv w)


instance SpreadX Witness where
 spreadX kenv tenv ww
  = let down = spreadX kenv tenv 
    in case ww of
        WCon  wc         -> WCon  (down wc)
        WVar  u          -> WVar  (down u)
        WApp  w1 w2      -> WApp  (down w1) (down w2)
        WJoin w1 w2      -> WJoin (down w1) (down w2)
        WType t1         -> WType (spreadT kenv t1)


instance SpreadX WiCon where
 spreadX kenv tenv wc
  = let down = spreadX kenv tenv
    in case wc of
        WiConBound u     -> WiConBound (down u)
        WiConBuiltin{}   -> wc


instance SpreadX Bind where
 spreadX kenv _tenv bb
  = case bb of
        BName n t        -> BName n (spreadT kenv t)
        BAnon t          -> BAnon (spreadT kenv t)
        BNone t          -> BNone (spreadT kenv t)


instance SpreadX Bound where
 spreadX kenv tenv uu
  | Just t'     <- Env.lookup uu tenv
  = case uu of
        UIx ix _         -> UIx ix t'
        UPrim n _        -> UPrim n t'

        UName n _
         -> if Env.isPrim tenv n 
                 then UPrim n (spreadT kenv t')
                 else UName n (spreadT kenv t')

  | otherwise   = uu        


