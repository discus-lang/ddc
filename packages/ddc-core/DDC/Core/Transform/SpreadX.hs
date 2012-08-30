
-- | Spread type annotations from binders and the environment into bound
--   occurrences of variables and constructors.
module DDC.Core.Transform.SpreadX
        (SpreadX(..))
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Transform.SpreadT
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified Data.Map               as Map

class SpreadX (c :: * -> *) where

 -- | Spread type annotations from binders and the environment into bound
 --   occurrences of variables and constructors.
 --
 --   Also convert `Bound`s to `UPrim` form if the environment says that
 --   they are primitive.
 spreadX :: forall n. Ord n
         => Env n -> Env n -> c n -> c n


-- Module ---------------------------------------------------------------------
instance SpreadX (Module a) where
 spreadX kenv tenv mm@ModuleCore{}
        = mm
        { moduleExportKinds = Map.map (spreadT kenv)  (moduleExportKinds mm)
        , moduleExportTypes = Map.map (spreadT kenv)  (moduleExportTypes mm)
        , moduleImportKinds = Map.map (liftSnd (spreadT kenv))  (moduleImportKinds mm)
        , moduleImportTypes = Map.map (liftSnd (spreadT kenv))  (moduleImportTypes mm)
        , moduleBody        = spreadX kenv tenv (moduleBody mm) }

        where liftSnd f (x, y) = (x, f y)
        

-------------------------------------------------------------------------------
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


instance SpreadX DaCon where
 spreadX _kenv tenv dc
  = case daConName dc of
        DaConUnit       -> dc

        DaConNamed n
         -> let u | Env.isPrim tenv n   = UPrim n (daConType dc)
                  | otherwise           = UName n

            in  case Env.lookup u tenv of
                 Just t' -> dc { daConType = t' }
                 Nothing -> dc -- error $ "spreadX: data constructor is not in type environment."


instance SpreadX (Cast a) where
 spreadX kenv tenv cc
  = let down = spreadX kenv tenv 
    in case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  (spreadT kenv eff)
        CastWeakenClosure xs    -> CastWeakenClosure (map down xs)
        CastPurify w            -> CastPurify        (down w)
        CastForget w            -> CastForget        (down w)


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

        LLetRegions b bs
         -> let b'       = map (spreadT kenv) b
                kenv'    = Env.extends b' kenv
                bs'      = map (spreadX kenv' tenv) bs
            in  LLetRegions b' bs'

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
  = case wc of
        WiConBound (UName n) _
         -> case Env.envPrimFun tenv n of
                Nothing -> wc
                Just t  
                 -> let t'      = spreadT kenv t
                    in  WiConBound (UPrim n t') t'

        _                -> wc

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
        UIx ix          -> UIx   ix

        UName n
         -> if Env.isPrim tenv n 
                 then UPrim n (spreadT kenv t')
                 else UName n

        UPrim n _       -> UPrim n t'

  | otherwise   = uu        


