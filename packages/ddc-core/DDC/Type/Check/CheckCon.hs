{-# OPTIONS_HADDOCK hide #-}
module DDC.Type.Check.CheckCon
        ( takeKindOfTyCon
        , takeSortOfKiCon
        , kindOfTwCon
        , kindOfTcCon)
where
import DDC.Type.Exp
import DDC.Type.Compounds


-- | Take the kind of a `TyCon`, if there is one.
takeKindOfTyCon :: TyCon n -> Maybe (Kind n)
takeKindOfTyCon tt
 = case tt of        
        -- Sorts don't have a higher classification.
        TyConSort    _  -> Nothing
 
        TyConKind    kc -> takeSortOfKiCon kc
        TyConWitness tc -> Just $ kindOfTwCon tc
        TyConComp    tc -> Just $ kindOfTcCon tc
        TyConBound   u  -> Just $ typeOfBound u


-- | Take the superkind of an atomic kind constructor.
--
--   * Yields `Nothing` for the kind function (~>) as it doesn't have a sort
--     without being fully applied.
takeSortOfKiCon :: KiCon -> Maybe (Sort n)
takeSortOfKiCon kc
 = case kc of
        KiConFun        -> Nothing
        KiConData       -> Just sComp
        KiConRegion     -> Just sComp
        KiConEffect     -> Just sComp
        KiConClosure    -> Just sComp
        KiConWitness    -> Just sProp


-- | Take the kind of a witness type constructor.
kindOfTwCon :: TwCon -> Kind n
kindOfTwCon tc
 = case tc of
        TwConImpl       -> kWitness `kFun` (kWitness `kFun` kWitness)
        TwConPure       -> kEffect  `kFun` kWitness
        TwConEmpty      -> kClosure `kFun` kWitness
        TwConGlobal     -> kRegion  `kFun` kWitness
        TwConDeepGlobal -> kData    `kFun` kWitness
        TwConConst      -> kRegion  `kFun` kWitness
        TwConDeepConst  -> kData    `kFun` kWitness
        TwConMutable    -> kRegion  `kFun` kWitness
        TwConDeepMutable-> kData    `kFun` kWitness
        TwConLazy       -> kRegion  `kFun` kWitness
        TwConHeadLazy   -> kData    `kFun` kWitness
        TwConManifest   -> kRegion  `kFun` kWitness
        TwConDistinct n -> kFuns (replicate n kRegion) kWitness


-- | Take the kind of a computation type constructor.
kindOfTcCon :: TcCon -> Kind n
kindOfTcCon tc
 = case tc of
        TcConFun        -> [kData, kEffect, kClosure, kData] `kFuns` kData
        TcConRead       -> kRegion  `kFun` kEffect
        TcConHeadRead   -> kData    `kFun` kEffect
        TcConDeepRead   -> kData    `kFun` kEffect
        TcConWrite      -> kRegion  `kFun` kEffect
        TcConDeepWrite  -> kData    `kFun` kEffect
        TcConAlloc      -> kRegion  `kFun` kEffect
        TcConDeepAlloc  -> kData    `kFun` kEffect
        TcConUse        -> kRegion  `kFun` kClosure
        TcConDeepUse    -> kData    `kFun` kClosure


