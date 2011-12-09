{-# OPTIONS_HADDOCK hide #-}
module DDC.Type.Check.CheckCon
        ( sortOfKiCon
        , kindOfTwCon
        , kindOfTcCon)
where
import DDC.Type.Exp
import DDC.Type.Compounds


-- | Take the superkind of an atomic kind constructor.
--
--   * Yields `Nothing` for the kind function (~>) as it doesn't have a sort
--     without being fully applied.
sortOfKiCon :: KiCon -> Maybe (Sort n)
sortOfKiCon kc
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
        TwConConst      -> kRegion  `kFun` kWitness
        TwConDeepConst  -> kData    `kFun` kWitness
        TwConMutable    -> kRegion  `kFun` kWitness
        TwConDeepMutable-> kData    `kFun` kWitness
        TwConLazy       -> kRegion  `kFun` kWitness
        TwConHeadLazy   -> kData    `kFun` kWitness
        TwConDirect     -> kData    `kFun` kWitness
        TwConDistinct n -> kFuns (replicate n kRegion) kWitness
        TwConPure       -> kEffect  `kFun` kWitness
        TwConEmpty      -> kClosure `kFun` kWitness


-- | Take the kind of a computation type constructor.
kindOfTcCon :: TcCon -> Kind n
kindOfTcCon tc
 = case tc of
        TcConFun        -> [kData, kEffect, kClosure, kData] `kFuns` kData
        TcConRead       -> kRegion  `kFun` kEffect
        TcConDeepRead   -> kData    `kFun` kEffect
        TcConWrite      -> kRegion  `kFun` kEffect
        TcConDeepWrite  -> kData    `kFun` kEffect
        TcConAlloc      -> kRegion  `kFun` kEffect
        TcConShare      -> kRegion  `kFun` kClosure
        TcConDeepShare  -> kData    `kFun` kClosure



