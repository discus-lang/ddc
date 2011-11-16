{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Compounds
        ( -- * Sort Construction.
          sAny, sComp, sProp

          -- * Kind Construction.
        , kAny
        , kData, kRegion, kEffect, kClosure, kWitness
        , kFun,  kFuns
        
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
        
          -- * Type structure
        , tBot
        , tApp,     tApps
        , tImpl
        , tLam1,    tLams)
where
import DDC.Type.Exp


-- Level 3 constructors (sorts) -------------------------------------------------------------------
sAny            = TCon $ TConSort SoAny
sComp           = TCon $ TConSort SoComp
sProp           = TCon $ TConSort SoProp


-- Level 2 constructors (kinds) -------------------------------------------------------------------
kAny            = TCon $ TConKind KiConAny
kData           = TCon $ TConKind KiConData
kRegion         = TCon $ TConKind KiConRegion
kEffect         = TCon $ TConKind KiConEffect
kClosure        = TCon $ TConKind KiConClosure
kWitness        = TCon $ TConKind KiConWitness

-- | Build a kind function.
kFun  :: Type v -> Type v -> Type v
kFun k1 k2
        = (TCon (TConKind KiConFun) `TApp` k1) `TApp` k2

-- | Build some kind functions.
kFuns :: [Type v] -> Type v -> Type v
kFuns []  k1    = k1
kFuns ks  k1    
 = let  k       = last ks
        ks'     = init ks
   in   kFuns ks' k `kFun` k1


-- Level 1 constructors (value types) -------------------------------------------------------------
tRead           = TCon $ TConType $ TyConRead
tDeepRead       = TCon $ TConType $ TyConDeepRead
tWrite          = TCon $ TConType $ TyConWrite
tDeepWrite      = TCon $ TConType $ TyConDeepWrite
tAlloc          = TCon $ TConType $ TyConAlloc
tFree           = TCon $ TConType $ TyConFree
tDeepFree       = TCon $ TConType $ TyConDeepFree
tConst          = TCon $ TConType $ TyConConst
tDeepConst      = TCon $ TConType $ TyConDeepConst
tMutable        = TCon $ TConType $ TyConMutable
tDeepMutable    = TCon $ TConType $ TyConDeepMutable
tLazy           = TCon $ TConType $ TyConLazy
tHeadLazy       = TCon $ TConType $ TyConHeadLazy
tDirect         = TCon $ TConType $ TyConDirect
tDistinct n     = TCon $ TConType $ TyConDistinct n
tPure           = TCon $ TConType $ TyConPure
tEmpty          = TCon $ TConType $ TyConEmpty


-- Type Structure ---------------------------------------------------------------------------------
tBot            = TBot

tApp            = TApp

tApps   :: Type n -> [Type n] -> Type n
tApps t1 ts     = foldl (flip TApp) t1 ts

-- | Build a witness implication type.
tImpl t1 t2      
        = ((TCon $ TConType $ TyConImpl) `tApp` t1) `tApp` t2


-- | Build an anonymous type abstraction, with a single parameter.
tLam1 :: Kind n -> (Type n -> Type n) -> Type n
tLam1 k f
 = let  b       = BVar NAnon   k
        u       = UVar (NIx 0) k
   in   TLam b (f (TVar u))


-- | Build an anonymous type abstraction, with several parameters.
tLams  :: [Kind n] -> ([Type n] -> Type n) -> Type n
tLams ks f
 = let  bs      = [BVar NAnon k          | k <- ks]
        us      = [TVar (UVar (NIx n) k) | k <- ks | n <- [0..]]
   in   foldr TLam (f $ reverse us) bs


-- Witnesses --------------------------------------------------------------------------------------
wApp            = WApp


