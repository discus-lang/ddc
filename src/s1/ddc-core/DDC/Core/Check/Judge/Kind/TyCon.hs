{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Kind.TyCon
        ( takeSortOfKiCon
        , kindOfTwCon
        , kindOfTcCon)
where
import DDC.Type.Exp.Simple


-- | Take the superkind of an atomic kind constructor.
--
--   Yields `Nothing` for the kind function (~>) as it doesn't have a sort
--   without being fully applied.
takeSortOfKiCon :: KiCon -> Maybe (Sort n)
takeSortOfKiCon kc
 = case kc of
        KiConFun        -> Nothing
        KiConData       -> Just sComp
        KiConRegion     -> Just sComp
        KiConEffect     -> Just sComp
        KiConClosure    -> Just sComp
        KiConWitness    -> Just sProp
        KiConRow        -> Just sComp


-- | Take the kind of a witness type constructor.
kindOfTwCon :: TwCon -> Kind n
kindOfTwCon tc
 = case tc of
        TwConImpl        -> kWitness  `kFun`  kWitness `kFun` kWitness
        TwConPure        -> kEffect   `kFun`  kWitness
        TwConConst       -> kRegion   `kFun`  kWitness
        TwConMutable     -> kRegion   `kFun`  kWitness
        TwConDisjoint    -> kEffect   `kFun`  kEffect  `kFun`  kWitness
        TwConDistinct n  -> (replicate n kRegion)      `kFuns` kWitness


-- | Take the kind of a computation type constructor.
kindOfTcCon :: TcCon -> Kind n
kindOfTcCon tc
 = case tc of
        TcConUnit        -> kData
        TcConSusp        -> kEffect  `kFun` kData `kFun` kData
        TcConFunExplicit -> kData    `kFun` kData `kFun` kData
        TcConFunImplicit -> kData    `kFun` kData `kFun` kData
        TcConRecord ns   -> map (const kData) ns  `kFuns` kData
        TcConT           -> kRow     `kFun` kData
        TcConR           -> kRow     `kFun` kData
        TcConV           -> kRow     `kFun` kData
        TcConRead        -> kRegion  `kFun` kEffect
        TcConWrite       -> kRegion  `kFun` kEffect
        TcConAlloc       -> kRegion  `kFun` kEffect
