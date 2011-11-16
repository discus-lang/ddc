
module DDC.Type.Check.Con
        ( sortOfKiCon
        , kindOfTyCon
        , typeOfWiCon)
where
import DDC.Type.Exp
import DDC.Type.Compounds


-- | Take the superkind of an atomic kind constructor.
--   The kind function (~>) is handled separately because it doesn't have a sort
--   without being fully applied.
sortOfKiCon :: KiCon -> Sort n
sortOfKiCon kc
 = case kc of
        KiConData       -> sComp
        KiConRegion     -> sComp
        KiConEffect     -> sComp
        KiConClosure    -> sComp
        KiConWitness    -> sProp


-- | Take the kind of an type constructor.
kindOfTyCon :: TyCon n -> Kind n
kindOfTyCon tc
 = case tc of
        TyConFun        -> [kData, kData, kEffect, kClosure] `kFuns` kData
        TyConData _ k   -> k
        TyConRead       -> kRegion  `kFun` kEffect
        TyConDeepRead   -> kData    `kFun` kEffect
        TyConWrite      -> kRegion  `kFun` kEffect
        TyConDeepWrite  -> kData    `kFun` kEffect
        TyConAlloc      -> kRegion  `kFun` kEffect
        TyConFree       -> kRegion  `kFun` kClosure
        TyConDeepFree   -> kData    `kFun` kClosure
        TyConImpl       -> kWitness `kFun` (kWitness `kFun` kWitness)
        TyConConst      -> kRegion  `kFun` kWitness
        TyConDeepConst  -> kData    `kFun` kWitness
        TyConMutable    -> kRegion  `kFun` kWitness
        TyConDeepMutable-> kData    `kFun` kWitness
        TyConLazy       -> kRegion  `kFun` kWitness
        TyConHeadLazy   -> kData    `kFun` kWitness
        TyConDirect     -> kData    `kFun` kWitness
        TyConDistinct n -> kFuns (replicate n kRegion) kWitness
        TyConPure       -> kEffect  `kFun` kWitness
        TyConEmpty      -> kClosure `kFun` kWitness


-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon -> Type n
typeOfWiCon wc
 = case wc of
        WiConMkPure     -> tPure  `tApp` tBot kEffect
        WiConMkEmpty    -> tEmpty `tApp` tBot kClosure

        WiConMkConst    
         -> tForall kRegion $ \r -> tConst r

        WiConMkMutable
         -> tForall kRegion $ \r -> tMutable `tApp` r

        WiConMkLazy
         -> tForall kRegion $ \r -> tLazy    `tApp` r

        WiConMkDirect
         -> tForall kRegion $ \r -> tDirect  `tApp` r

        WiConMkPurify
         -> tForall kRegion $ \r -> (tConst r) `tImpl`  (tPure  `tApp` (tRead `tApp` r))

        WiConMkShare
         -> tForall kRegion $ \r -> (tConst r)  `tImpl` (tEmpty `tApp` (tFree `tApp` r))

        WiConMkDistinct n
         -> tForalls (replicate n kRegion) $ \rs -> (tDistinct n) `tApps` rs

