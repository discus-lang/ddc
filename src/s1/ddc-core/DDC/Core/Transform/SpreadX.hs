
module DDC.Core.Transform.SpreadX
        (SpreadX(..))
where
import DDC.Core.Module
import DDC.Core.Exp.Annot
import DDC.Type.Transform.SpreadT
import Control.Monad
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


---------------------------------------------------------------------------------------------------
instance SpreadX (Module a) where
 spreadX kenv tenv mm@ModuleCore{}
  = let liftSnd f (x, y) = (x, f y)
    in  ModuleCore
        { moduleName
                = moduleName mm

        , moduleIsHeader
                = moduleIsHeader mm

        , moduleExportTypes
                = map (liftSnd $ spreadExportTypeT kenv)
                $ moduleExportTypes mm

        , moduleExportValues
                = map (liftSnd $ spreadExportValueT kenv)
                $ moduleExportValues mm

        , moduleImportTypes
                = map (liftSnd $ spreadImportTypeT kenv tenv)
                $ moduleImportTypes mm

        , moduleImportCaps
                = map (liftSnd $ spreadImportCapX kenv tenv)
                $ moduleImportCaps mm

        , moduleImportValues
                = map (liftSnd $ spreadImportValueX kenv tenv)
                $ moduleImportValues mm

        , moduleImportDataDefs
                = map (liftSnd $ spreadT kenv)
                $ moduleImportDataDefs mm

        , moduleImportTypeDefs
                = map (\(n, (k, t)) -> (n, (spreadT kenv k, spreadT kenv t)))
                $ moduleImportTypeDefs mm

        , moduleDataDefsLocal
                = map (liftSnd $ spreadT kenv)
                $ moduleDataDefsLocal mm

        , moduleTypeDefsLocal
                = map (\(n, (k, t)) -> (n, (spreadT kenv k, spreadT kenv t)))
                $ moduleTypeDefsLocal mm

        , moduleBody
                 = spreadX kenv tenv
                 $ moduleBody mm
        }


---------------------------------------------------------------------------------------------------
spreadExportTypeT kenv esrc
  = case esrc of
        ExportTypeLocal n t
         -> ExportTypeLocal n (spreadT kenv t)

        ExportTypeLocalNoKind n
         -> ExportTypeLocalNoKind n


---------------------------------------------------------------------------------------------------
spreadExportValueT kenv esrc
  = case esrc of
        ExportValueLocal n t mArity
         -> ExportValueLocal n (spreadT kenv t) mArity

        ExportValueLocalNoType n
         -> ExportValueLocalNoType n


---------------------------------------------------------------------------------------------------
spreadImportTypeT kenv _tenv isrc
  = case isrc of
        ImportTypeAbstract t
         -> ImportTypeAbstract (spreadT kenv t)

        ImportTypeBoxed t
         -> ImportTypeBoxed    (spreadT kenv t)


---------------------------------------------------------------------------------------------------
spreadImportCapX kenv _tenv isrc
  = case isrc of
        ImportCapAbstract t
         -> ImportCapAbstract   (spreadT kenv t)


---------------------------------------------------------------------------------------------------
spreadImportValueX kenv _tenv isrc
  = case isrc of
        ImportValueModule mn n t mArity
         -> ImportValueModule   mn n (spreadT kenv t) mArity

        ImportValueSea n t
         -> ImportValueSea n    (spreadT kenv t)


---------------------------------------------------------------------------------------------------
instance SpreadX (Exp a) where
 spreadX kenv tenv xx
  = let down x = spreadX kenv tenv x
    in case xx of
        XVar  a u       -> XVar  a (down u)
        XPrim a p       -> XPrim a p
        XCon  a d       -> XCon  a (spreadDaCon kenv tenv d)
        XApp  a x1 x2   -> XApp  a (down x1) (down x2)

        XAbs a (MType b) x
         -> let b'      = spreadT kenv b
            in  XAbs a (MType b')     (spreadX (Env.extend b' kenv) tenv x)

        XAbs a (MTerm b) x
         -> let b'      = down b
            in  XAbs a (MTerm b')     (spreadX kenv (Env.extend b' tenv) x)

        XAbs a (MImplicit b) x
         -> let b'      = down b
            in  XAbs a (MImplicit b') (spreadX kenv (Env.extend b' tenv) x)

        XLet a lts x
         -> let lts'    = down lts
                kenv'   = Env.extends (specBindsOfLets   lts') kenv
                tenv'   = Env.extends (valwitBindsOfLets lts') tenv
            in  XLet a lts' (spreadX kenv' tenv' x)

        XCase a x alts  -> XCase    a (down x) (map down alts)
        XCast a c x     -> XCast    a (down c) (down x)


instance SpreadX (Arg a) where
 spreadX kenv tenv aa
  = case aa of
        RType t         -> RType     $ spreadT kenv t
        RTerm x         -> RTerm     $ spreadX kenv tenv x
        RImplicit x     -> RImplicit $ spreadX kenv tenv x
        RWitness  x     -> RWitness  $ spreadX kenv tenv x


---------------------------------------------------------------------------------------------------
spreadDaCon _kenv tenv dc
  = case dc of
        DaConUnit       -> dc
        DaConRecord{}   -> dc

        DaConPrim n _
         -> let u | Env.isPrim tenv n   = UPrim n
                  | otherwise           = UName n

            in  case Env.lookup u tenv of
                 Just t' -> dc { daConType = t' }
                 Nothing -> dc

        DaConBound n
         | Env.isPrim tenv n
         , Just t'      <- Env.lookup (UPrim n) tenv
         -> DaConPrim n t'

         | otherwise
         -> DaConBound n


---------------------------------------------------------------------------------------------------
instance SpreadX (Cast a) where
 spreadX kenv tenv cc
  = let down x = spreadX kenv tenv x
    in case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  (spreadT kenv eff)
        CastPurify w            -> CastPurify        (down w)
        CastBox                 -> CastBox
        CastRun                 -> CastRun


---------------------------------------------------------------------------------------------------
instance SpreadX Pat where
 spreadX kenv tenv pat
  = let down x   = spreadX kenv tenv x
    in case pat of
        PDefault        -> PDefault
        PData u bs      -> PData (spreadDaCon kenv tenv u) (map down bs)


---------------------------------------------------------------------------------------------------
instance SpreadX (Alt a) where
 spreadX kenv tenv alt
  = case alt of
        AAlt p x
         -> let p'       = spreadX kenv tenv p
                tenv'    = Env.extends (bindsOfPat p') tenv
            in  AAlt p' (spreadX kenv tenv' x)


---------------------------------------------------------------------------------------------------
instance SpreadX (Lets a) where
 spreadX kenv tenv lts
  = let down x = spreadX kenv tenv x
    in case lts of
        LLet b x
         -> LLet (down b) (down x)

        LRec bxs
         -> let (bs, xs) = unzip bxs
                bs'      = map (spreadX kenv tenv) bs
                tenv'    = Env.extends bs' tenv
                xs'      = map (spreadX kenv tenv') xs
             in LRec (zip bs' xs')

        LPrivate b mT bs
         -> let b'       = map (spreadT kenv) b
                mT'      = liftM (spreadT kenv) mT
                kenv'    = Env.extends b' kenv
                bs'      = map (spreadX kenv' tenv) bs
            in  LPrivate b' mT' bs'


---------------------------------------------------------------------------------------------------
instance SpreadX (Witness a) where
 spreadX kenv tenv ww
  = let down = spreadX kenv tenv
    in case ww of
        WCon  a wc       -> WCon  a (down wc)
        WVar  a u        -> WVar  a (down u)
        WApp  a w1 w2    -> WApp  a (down w1) (down w2)
        WType a t1       -> WType a (spreadT kenv t1)


---------------------------------------------------------------------------------------------------
instance SpreadX WiCon where
 spreadX kenv tenv wc
  = case wc of
        WiConBound (UName n) _
         -> case Env.envPrimFun tenv n of
                Nothing -> wc
                Just t
                 -> let t'      = spreadT kenv t
                    in  WiConBound (UPrim n) t'

        _                -> wc


---------------------------------------------------------------------------------------------------
instance SpreadX Bind where
 spreadX kenv _tenv bb
  = case bb of
        BName n t        -> BName n (spreadT kenv t)
        BAnon t          -> BAnon (spreadT kenv t)
        BNone t          -> BNone (spreadT kenv t)


---------------------------------------------------------------------------------------------------
instance SpreadX Bound where
 spreadX _kenv tenv uu
  = case uu of
        UIx ix          -> UIx   ix
        UName n
         -> if Env.isPrim tenv n
                 then UPrim n
                 else UName n
        UPrim n         -> UPrim n


