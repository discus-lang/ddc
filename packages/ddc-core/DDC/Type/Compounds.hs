{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Compounds
        ( takeNameOfBind
        , kindOfBind
        , replaceKindOfBind

        , takeNameOfBound
        , kindOfBound
        , replaceKindOfBound

        -- * Type structure.
        , tBot
        , tApp,     ($:)
        , tApps
        , tForall
        , tForalls

        -- * Function constructos.
        , kFun,     (~>>)
        , kFuns
        , tFun,     (->>)
        , tImpl

          -- * Sort Construction.
        , sComp, sProp

          -- * Kind Construction.
        , kData, kRegion, kEffect, kClosure, kWitness

          -- * Type Construction.
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
        
          -- * Witness Construction
        , wApp        
        )
where
import DDC.Type.Exp

-- Names, Binds and Bounds ------------------------------------------------------------------------
-- | Take the variable name of a bind.
--   If this is an anonymous variable then there won't be a name.
takeNameOfBind  :: Bind n -> Maybe n
takeNameOfBind  (BName n _)     = Just n
takeNameOfBind  (BAnon   _)     = Nothing


-- | Take the kind of a bind.
kindOfBind :: Bind n -> Kind n
kindOfBind (BName _ k)          = k
kindOfBind (BAnon   k)          = k


-- | Replace the kind of a bind with a new one.
replaceKindOfBind :: Kind n -> Bind n -> Bind n
replaceKindOfBind k (BName n _) = BName n k
replaceKindOfBind k (BAnon   _) = BAnon k


-- | Take the variable name of bound variable.
--   If this is an anonymous variable then there won't be a name.
takeNameOfBound :: Bound n -> Maybe n
takeNameOfBound (UName n _)     = Just n
takeNameOfBound (UIx _ _)       = Nothing


-- | Take the kind of a bound variable.
kindOfBound :: Bind n -> Kind n
kindOfBound (BName _ k)         = k
kindOfBound (BAnon   k)         = k


-- | Replace the kind of a bound with a new one.
replaceKindOfBound :: Kind n -> Bound n -> Bound n
replaceKindOfBound k (UName n _) = UName n k
replaceKindOfBound k (UIx i _)   = UIx i k


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
kFun k1 k2      = (TCon TConKindFun `TApp` k1) `TApp` k2
(~>>)           = kFun


-- | Build some kind functions.
kFuns :: [Kind n] -> Kind n -> Kind n
kFuns []     k1    = k1
kFuns (k:ks) k1    = k `kFun` kFuns ks k1


-- | Build a value type function, 
--   with the provided effect and closure.
tFun    :: Type n -> Type n -> Effect n -> Closure n -> Type n
tFun t1 t2 eff clo
        = (TCon $ TConType $ TyConBuiltin TyConFun) `tApps` [t1, t2, eff, clo]

-- | Build a pure and empty value type function.
tFunPE, (->>)   :: Type n -> Type n -> Type n
tFunPE t1 t2    = tFun t1 t2 (tBot kEffect) (tBot kClosure)
(->>)           = tFunPE


-- | Build a witness implication type.
tImpl :: Type n -> Type n -> Type n
tImpl t1 t2      
        = ((TCon $ TConType $ TyConBuiltin $ TyConImpl) `tApp` t1) `tApp` t2



-- Level 3 constructors (sorts) -------------------------------------------------------------------
sComp           = TCon $ TConSort SoConComp
sProp           = TCon $ TConSort SoConProp


-- Level 2 constructors (kinds) -------------------------------------------------------------------
kData           = TCon $ TConKind KiConData
kRegion         = TCon $ TConKind KiConRegion
kEffect         = TCon $ TConKind KiConEffect
kClosure        = TCon $ TConKind KiConClosure
kWitness        = TCon $ TConKind KiConWitness

-- Level 1 constructors (value types) -------------------------------------------------------------
tRead           = tBuiltin1 TyConRead
tDeepRead       = tBuiltin1 TyConDeepRead
tWrite          = tBuiltin1 TyConWrite
tDeepWrite      = tBuiltin1 TyConDeepWrite
tAlloc          = tBuiltin1 TyConAlloc
tFree           = tBuiltin1 TyConFree
tDeepFree       = tBuiltin1 TyConDeepFree
tConst          = tBuiltin1 TyConConst
tDeepConst      = tBuiltin1 TyConDeepConst
tMutable        = tBuiltin1 TyConMutable
tDeepMutable    = tBuiltin1 TyConDeepMutable
tLazy           = tBuiltin1 TyConLazy
tHeadLazy       = tBuiltin1 TyConHeadLazy
tDirect         = tBuiltin1 TyConDirect
tPure           = tBuiltin1 TyConPure
tEmpty          = tBuiltin1 TyConEmpty
tDistinct       = tBuiltinN TyConDistinct 

tBuiltin1 tc t  = (TCon $ TConType $ TyConBuiltin tc) `tApp` t
tBuiltinN tc ts = (TCon $ TConType $ TyConBuiltin (tc (length ts))) `tApps` ts

-- Witnesses --------------------------------------------------------------------------------------
wApp            = WApp

