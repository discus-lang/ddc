
-- | Spreading of type annotations from binders and the environment into bound
--   variables and constructors.
module DDC.Core.Transform.SpreadX
        (SpreadX(..))
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Transform.SpreadT
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Pretty        as P


class SpreadX (c :: * -> *) where
 -- | Spread type annotations from the environment and binders
 --   into variables at the leaves.
 spreadX :: forall n. (Ord n, Show n, P.Pretty n) 
         => Env n -> Env n -> c n -> c n


instance SpreadX (Exp a) where
 spreadX kenv tenv xx
  = case xx of
        XVar a u        -> XVar a (spreadX kenv tenv u)
        XCon a u        -> XCon a (spreadX kenv tenv u)
        XApp a x1 x2    -> XApp a (spreadX kenv tenv x1) (spreadX kenv tenv x2)

        XLAM a b x
         -> let b'      = spreadT kenv b
            in  XLAM a b' (spreadX (Env.extend b' kenv) tenv x)

        XLam a b x      
         -> let b'      = spreadX kenv tenv b
            in  XLam a b' (spreadX kenv (Env.extend b' tenv) x)
            
        XLet a lts x
         -> let lts'    = spreadX kenv tenv lts
                kenv'   = Env.extends (specBindsOfLets   lts') kenv
                tenv'   = Env.extends (valwitBindsOfLets lts') tenv
            in  XLet a lts' (spreadX kenv' tenv' x)
         
        XCase a x alts
         -> let x'      = spreadX kenv tenv x
                alts'   = map (spreadX kenv tenv) alts
            in  XCase a x' alts'

        XCast a c x     -> XCast a  (spreadX kenv tenv c) (spreadX kenv tenv x)
        
        XType t         -> XType    (spreadT kenv t)

        XWitness w      -> XWitness (spreadX kenv tenv w)


instance SpreadX Cast where
 spreadX kenv tenv cc
  = case cc of
        CastWeakenEffect eff    
         -> CastWeakenEffect  (spreadT kenv eff)

        CastWeakenClosure clo   
         -> CastWeakenClosure (spreadT kenv clo)

        CastPurify w
         -> CastPurify        (spreadX kenv tenv w)

        CastForget w
         -> CastForget        (spreadX kenv tenv w)


instance SpreadX Pat where
 spreadX kenv tenv pat
  = case pat of
        PDefault        -> PDefault
        PData u bs      -> PData (spreadX kenv tenv u) 
                                 (map (spreadX kenv tenv) bs)


instance SpreadX (Alt a) where
 spreadX kenv tenv alt
  = case alt of
        AAlt p x
         -> let p'      = spreadX kenv tenv p
                tenv'   = Env.extends (bindsOfPat p') tenv
            in  AAlt p' (spreadX kenv tenv' x)


instance SpreadX (Lets a) where
 spreadX kenv tenv lts
  = case lts of
        LLet m b x     
         -> let m'      = spreadX kenv tenv m
                b'      = spreadX kenv tenv b
                x'      = spreadX kenv tenv x
            in  LLet m' b' x'
        
        LRec bxs
         -> let (bs, xs) = unzip bxs
                bs'      = map (spreadX kenv tenv) bs
                tenv'    = Env.extends bs' tenv
                xs'      = map (spreadX kenv tenv') xs
             in LRec (zip bs' xs')

        LLetRegion b bs
         -> let b'      = spreadT kenv b
                kenv'   = Env.extend b' kenv
                bs'     = map (spreadX kenv' tenv) bs
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
  = case ww of
        WCon  wicon     -> WCon wicon
        WVar  u         -> WVar  (spreadX kenv tenv u)
        WApp  w1 w2     -> WApp  (spreadX kenv tenv w1) (spreadX kenv tenv w2)
        WJoin w1 w2     -> WJoin (spreadX kenv tenv w1) (spreadX kenv tenv w2)
        WType t1        -> WType (spreadT kenv t1)


instance SpreadX Bind where
 spreadX kenv _tenv bb
  = case bb of
        BName n t       -> BName n (spreadT kenv t)
        BAnon t         -> BAnon (spreadT kenv t)
        BNone t         -> BNone (spreadT kenv t)


instance SpreadX Bound where
 spreadX _kenv tenv uu
  = case uu of
        UIx ix _      
         | Just t'       <- Env.lookup uu tenv
         -> UIx ix t'
         
        UName n _
         | Just t'      <- Env.lookup uu tenv
         -> if Env.isPrim tenv n 
                 then UPrim n t'                         -- TODO: recursively spread into dropped type, but do occ check.
                 else UName n t'
                 
        UPrim n _
         | Just t'      <- Env.lookup uu tenv
         -> UPrim n t'
        
        _ -> uu
