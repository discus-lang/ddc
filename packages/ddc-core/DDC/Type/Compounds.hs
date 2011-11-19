{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Compounds
        ( takeNameOfBind,  kindOfBind
        , takeNameOfBound, kindOfBound

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
takeNameOfBind  :: Bind v c -> Maybe v
takeNameOfBind  (BName v _)     = Just v
takeNameOfBind  (BAnon   _)     = Nothing

-- | Take the kind of a bind.
kindOfBind :: Bind v c -> Kind v c
kindOfBind (BName _ k)          = k
kindOfBind (BAnon   k)          = k


-- | Take the variable name of bound variable.
--   If this is an anonymous variable then there won't be a name.
takeNameOfBound :: Bound v c -> Maybe v
takeNameOfBound (UName v _)     = Just v
takeNameOfBound (UIx _ _)       = Nothing

-- | Take the kind of a bound variable.
kindOfBound :: Bind v c -> Kind v c
kindOfBound (BName _ k)         = k
kindOfBound (BAnon   k)         = k


-- Type Structure ---------------------------------------------------------------------------------
tBot            = TBot
tApp            = TApp
($:)            = TApp

tApps   :: Type v c -> [Type v c] -> Type  v c
tApps t1 ts     = foldl TApp t1 ts


-- | Build an anonymous type abstraction, with a single parameter.
tForall :: Kind v c -> (Type v c -> Type v c) -> Type v c
tForall k f
        = TForall (BAnon k) (f (TVar (UIx 0 k)))


-- | Build an anonymous type abstraction, with several parameters.
tForalls  :: [Kind v c] -> ([Type v c] -> Type v c) -> Type v c
tForalls ks f
 = let  bs      = [BAnon k | k <- ks]
        us      = [TVar (UIx n  k) | k <- ks | n <- [0..]]
   in   foldr TForall (f us) bs


-- Function Constructors --------------------------------------------------------------------------
-- | Build a kind function.
kFun, (~>>) :: Type v c -> Type v c -> Type v c
kFun k1 k2      = (TCon TConKindFun `TApp` k1) `TApp` k2
(~>>)           = kFun


-- | Build some kind functions.
kFuns :: [Type v c] -> Type v c -> Type v c
kFuns []  k1    = k1
kFuns ks  k1    
 = let  k       = last ks
        ks'     = init ks
   in   kFuns ks' k `kFun` k1


-- | Build a value type function, 
--   with the provided effect and closure.
tFun    :: Type v c -> Type v c -> Effect v c -> Closure v c -> Type v c
tFun t1 t2 eff clo
        = TCon (TConType TyConFun) `tApps` [t1, t2, eff, clo]

(->>)   :: Type v c -> Type v c -> Type v c
(->>) t1 t2     = tFun t1 t2 (tBot kEffect) (tBot kClosure)


-- | Build a witness implication type.
tImpl :: Type v c -> Type v c -> Type v c
tImpl t1 t2      
        = ((TCon $ TConType $ TyConImpl) `tApp` t1) `tApp` t2



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
tRead        t  = (TCon $ TConType $ TyConRead)        `tApp` t
tDeepRead    t  = (TCon $ TConType $ TyConDeepRead)    `tApp` t
tWrite       t  = (TCon $ TConType $ TyConWrite)       `tApp` t
tDeepWrite   t  = (TCon $ TConType $ TyConDeepWrite)   `tApp` t
tAlloc       t  = (TCon $ TConType $ TyConAlloc)       `tApp` t
tFree        t  = (TCon $ TConType $ TyConFree)        `tApp` t
tDeepFree    t  = (TCon $ TConType $ TyConDeepFree)    `tApp` t
tConst       t  = (TCon $ TConType $ TyConConst)       `tApp` t
tDeepConst   t  = (TCon $ TConType $ TyConDeepConst)   `tApp` t
tMutable     t  = (TCon $ TConType $ TyConMutable)     `tApp` t
tDeepMutable t  = (TCon $ TConType $ TyConDeepMutable) `tApp` t
tLazy        t  = (TCon $ TConType $ TyConLazy)        `tApp` t
tHeadLazy    t  = (TCon $ TConType $ TyConHeadLazy)    `tApp` t
tDirect      t  = (TCon $ TConType $ TyConDirect)      `tApp` t
tPure        t  = (TCon $ TConType $ TyConPure)        `tApp` t
tEmpty       t  = (TCon $ TConType $ TyConEmpty)       `tApp` t
tDistinct   ts  = (TCon $ TConType $ TyConDistinct (length ts)) `tApps` ts


-- Witnesses --------------------------------------------------------------------------------------
wApp            = WApp

