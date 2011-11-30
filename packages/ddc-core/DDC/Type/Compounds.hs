{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Compounds
        ( -- * Variable binders
          takeNameOfBind
        , typeOfBind
        , replaceTypeOfBind

        , takeNameOfBound
        , typeOfBound
        , replaceTypeOfBound

          -- * Type structure
        , tBot
        , tApp,     ($:)
        , tApps
        , tForall
        , tForalls

          -- * Function type construction
        , kFun,     (~>>)
        , kFuns
        , tFun,     (->>)
        , tImpl

          -- * Sort construction
        , sComp, sProp

          -- * Kind construction
        , kData, kRegion, kEffect, kClosure, kWitness

          -- * Type construction
        , tRead,    tDeepRead
        , tWrite,   tDeepWrite
        , tAlloc
        , tFree,    tDeepFree
        , tConst,   tDeepConst
        , tMutable, tDeepMutable
        , tLazy,    tHeadLazy
        , tDirect
        , tDistinct
        , tPure
        , tEmpty
        )
where
import DDC.Type.Exp

-- Names, Binds and Bounds ------------------------------------------------------------------------
-- | Take the variable name of a bind.
--   If this is an anonymous variable then there won't be a name.
takeNameOfBind  :: Bind n -> Maybe n
takeNameOfBind  (BName n _)     = Just n
takeNameOfBind  (BAnon   _)     = Nothing


-- | Take the type of a bind.
typeOfBind :: Bind n -> Type n
typeOfBind (BName _ k)          = k
typeOfBind (BAnon   k)          = k


-- | Replace the kind of a bind with a new one.
replaceTypeOfBind :: Type n -> Bind n -> Bind n
replaceTypeOfBind t (BName n _) = BName n t
replaceTypeOfBind t (BAnon   _) = BAnon t


-- | Take the variable name of bound variable.
--   If this is an anonymous variable then there won't be a name.
takeNameOfBound :: Bound n -> Maybe n
takeNameOfBound (UName n _)     = Just n
takeNameOfBound (UIx _ _)       = Nothing


-- | Take the type of a bound variable.
typeOfBound :: Bound n -> Type n
typeOfBound (UName _ k)         = k
typeOfBound (UIx _ k)           = k


-- | Replace the type of a bound with a new one.
replaceTypeOfBound :: Type n -> Bound n -> Bound n
replaceTypeOfBound t (UName n _) = UName n t
replaceTypeOfBound t (UIx i _)   = UIx i t


-- Type Structure ---------------------------------------------------------------------------------
tBot            = TBot
tApp            = TApp
($:)            = TApp

tApps   :: Type n -> [Type n] -> Type n
tApps t1 ts     = foldl TApp t1 ts


-- | Build an anonymous type abstraction, with a single parameter.
tForall :: Kind n -> (Type n -> Type n) -> Type n
tForall k f
        = TForall (BAnon k) (f (TVar (UIx 0 k)))


-- | Build an anonymous type abstraction, with several parameters.
tForalls  :: [Kind n] -> ([Type n] -> Type n) -> Type n
tForalls ks f
 = let  bs      = [BAnon k | k <- ks]
        us      = [TVar (UIx n  k) | k <- ks | n <- [0..]]
   in   foldr TForall (f us) bs


-- Function Constructors --------------------------------------------------------------------------
-- | Build a kind function.
kFun, (~>>) :: Kind n -> Kind n -> Kind n
kFun k1 k2      = ((TCon $ TyConKind KiConFun)`TApp` k1) `TApp` k2
(~>>)           = kFun


-- | Build some kind functions.
kFuns :: [Kind n] -> Kind n -> Kind n
kFuns []     k1    = k1
kFuns (k:ks) k1    = k `kFun` kFuns ks k1


-- | Build a value type function, 
--   with the provided effect and closure.
tFun    :: Type n -> Type n -> Effect n -> Closure n -> Type n
tFun t1 t2 eff clo
        = (TCon $ TyConComp TcConFun) `tApps` [t1, t2, eff, clo]

-- | Build a pure and empty value type function.
tFunPE, (->>)   :: Type n -> Type n -> Type n
tFunPE t1 t2    = tFun t1 t2 (tBot kEffect) (tBot kClosure)
(->>)           = tFunPE


-- | Build a witness implication type.
tImpl :: Type n -> Type n -> Type n
tImpl t1 t2      
        = ((TCon $ TyConWitness TwConImpl) `tApp` t1) `tApp` t2



-- Level 3 constructors (sorts) -------------------------------------------------------------------
sComp           = TCon $ TyConSort SoConComp
sProp           = TCon $ TyConSort SoConProp


-- Level 2 constructors (kinds) -------------------------------------------------------------------
kData           = TCon $ TyConKind KiConData
kRegion         = TCon $ TyConKind KiConRegion
kEffect         = TCon $ TyConKind KiConEffect
kClosure        = TCon $ TyConKind KiConClosure
kWitness        = TCon $ TyConKind KiConWitness


-- Level 1 constructors (witness and computation types) -------------------------------------------
tRead           = tcCon1 TcConRead
tDeepRead       = tcCon1 TcConDeepRead
tWrite          = tcCon1 TcConWrite
tDeepWrite      = tcCon1 TcConDeepWrite
tAlloc          = tcCon1 TcConAlloc
tFree           = tcCon1 TcConFree
tDeepFree       = tcCon1 TcConDeepFree

tConst          = twCon1 TwConConst
tDeepConst      = twCon1 TwConDeepConst
tMutable        = twCon1 TwConMutable
tDeepMutable    = twCon1 TwConDeepMutable
tLazy           = twCon1 TwConLazy
tHeadLazy       = twCon1 TwConHeadLazy
tDirect         = twCon1 TwConDirect
tPure           = twCon1 TwConPure
tEmpty          = twCon1 TwConEmpty
tDistinct       = twConN TwConDistinct 

tcCon1 tc t  = (TCon $ TyConComp    tc) `tApp` t
twCon1 tc t  = (TCon $ TyConWitness tc) `tApp` t
twConN tc ts = (TCon $ TyConWitness   (tc (length ts))) `tApps` ts


