{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Compounds
        ( typeOfBind
        , takeKeyOfBind
        , takeMoreOfBind

        , typeOfBound
        , nameUseOfBound
        
         -- * Sort Construction.
        , sComp, sProp

          -- * Kind Construction.
        , kData, kRegion, kEffect, kClosure, kWitness
        , kFun,     (~>>)
        , kFuns
        
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
        , tFun,     (->>)
        
          -- * Witness Construction
        , wApp
        
          -- * Type structure
        , tBot
        , tApp,     ($:)
        , tApps
        , tImpl
        , tForall,  tForalls)
where
import DDC.Type.Exp


-- Names, Binds and Bounds ------------------------------------------------------------------------
-- | Take the `Type` of a `Bind`.
typeOfBind :: Bind n -> Type n
typeOfBind bb
 = case bb of   
        BVar  _ t       -> t
        BMore _ t _     -> t


-- | Take the key of a bind, if there is one.
takeKeyOfBind :: Bind n -> Maybe n
takeKeyOfBind bb
 = case bb of
        BVar  (NDName n) _      -> Just n
        BMore (NDName n) _ _    -> Just n
        _                       -> Nothing


-- | Take the more-than constraint from a bind, if there is one.
takeMoreOfBind :: Bind n -> Maybe (Type n)
takeMoreOfBind bb
 = case bb of
        BVar{}                  -> Nothing
        BMore _ _ more          -> Just more


-- Bound ------------------------------------------------------------------------------------------
-- | Take the `Type` of a `Bound`
typeOfBound :: Bound n -> Type n
typeOfBound uu
 = case uu of   
        UVar  _ t       -> t
        UMore _ t _     -> t

nameUseOfBound :: Bound n -> NameUse n
nameUseOfBound uu
 = case uu of
        UVar   uu _             -> uu
        UMore  uu _ _           -> uu

-- Level 3 constructors (sorts) -------------------------------------------------------------------
sComp           = TCon $ TConSort SoComp
sProp           = TCon $ TConSort SoProp


-- Level 2 constructors (kinds) -------------------------------------------------------------------
kData           = TCon $ TConKind KiConData
kRegion         = TCon $ TConKind KiConRegion
kEffect         = TCon $ TConKind KiConEffect
kClosure        = TCon $ TConKind KiConClosure
kWitness        = TCon $ TConKind KiConWitness

-- | Build a kind function.
kFun, (~>>) :: Type v -> Type v -> Type v
kFun k1 k2      = (TCon TConKindFun `TApp` k1) `TApp` k2
(~>>)           = kFun

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
tConst    tr    = (TCon $ TConType $ TyConConst) `tApp` tr
tDeepConst      = TCon $ TConType $ TyConDeepConst
tMutable        = TCon $ TConType $ TyConMutable
tDeepMutable    = TCon $ TConType $ TyConDeepMutable
tLazy           = TCon $ TConType $ TyConLazy
tHeadLazy       = TCon $ TConType $ TyConHeadLazy
tDirect         = TCon $ TConType $ TyConDirect
tDistinct n     = TCon $ TConType $ TyConDistinct n
tPure           = TCon $ TConType $ TyConPure
tEmpty          = TCon $ TConType $ TyConEmpty


-- | Build a value type function, 
--   with the provided effect and closure.
tFun    :: Type n -> Type n -> Effect n -> Closure n -> Type n
tFun t1 t2 eff clo
        = TCon (TConType TyConFun) `tApps` [t1, t2, eff, clo]

(->>)   :: Type n -> Type n -> Type n
(->>) t1 t2     = tFun t1 t2 (tBot kEffect) (tBot kClosure)




-- Type Structure ---------------------------------------------------------------------------------
tBot            = TBot
tApp            = TApp
($:)            = TApp

tApps   :: Type n -> [Type n] -> Type n
tApps t1 ts     = foldl TApp t1 ts

-- | Build a witness implication type.
tImpl t1 t2      
        = ((TCon $ TConType $ TyConImpl) `tApp` t1) `tApp` t2


-- | Build an anonymous type abstraction, with a single parameter.
tForall :: Kind n -> (Type n -> Type n) -> Type n
tForall k f
 = let  b       = BVar NDAnon   k
        u       = UVar (NUIx 0) k
   in   TForall b (f (TVar u))


-- | Build an anonymous type abstraction, with several parameters.
tForalls  :: [Kind n] -> ([Type n] -> Type n) -> Type n
tForalls ks f
 = let  bs      = [BVar NDAnon k          | k <- ks]
        us      = [TVar (UVar (NUIx n) k) | k <- ks | n <- [0..]]
   in   foldr TForall (f us) bs


-- Witnesses --------------------------------------------------------------------------------------
wApp            = WApp

