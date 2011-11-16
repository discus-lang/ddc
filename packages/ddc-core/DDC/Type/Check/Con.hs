
module DDC.Type.Check.Con
        ( takeSortOfKiCon
        , kindOfTyCon
        , typeOfWiCon)
where
import DDC.Type.Exp
import DDC.Type.Compounds


-- | Take the superkind of an atomic kind constructor.
--
--   * The 'KiConAny' constructor has no superkind.
--
--   * The 'KiConFun' constructor is handled by a specific rule.
takeSortOfKiCon :: KiCon -> Maybe (Sort n)
takeSortOfKiCon kc
 = case kc of
        KiConAny        -> Nothing 
        KiConFun        -> Nothing
        KiConData       -> Just sComp
        KiConRegion     -> Just sComp
        KiConEffect     -> Just sComp
        KiConClosure    -> Just sComp
        KiConWitness    -> Just sProp


-- | Take the kind of an type constructor.
kindOfTyCon :: TyCon n -> Kind n
kindOfTyCon tc
 = case tc of
        TyConFun        -> [kData, kData, kEffect, kClosure] `kFuns` kData
        TyConData _ k   -> k
        TyConRead       -> kRegion  `kFun` kEffectret
        TyConDeepRead   -> kData    `kFun` kEffect
        TyConWrite      -> kRegion  `kFun` kEffect
        TyConDeepWrite  -> kData    `kFun` kEffect
        TyConAlloc      -> kRegion  `kFun` kEffect
        TyConFree       -> kRegion  `kFun` kClosure
        TyConDeepFree   -> kData    `kFun` kClosure
        TyConImpl       -> kWitness `kFun` kWitness `kFun` kWitness
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
         -> tLam1 kRegion $ \r -> tConst   `tApp` r

        WiConMkMutable
         -> tLam1 kRegion $ \r -> tMutable `tApp` r

        WiConMkLazy
         -> tLam1 kRegion $ \r -> tLazy    `tApp` r

        WiConMkDirect
         -> tLam1 kRegion $ \r -> tDirect  `tApp` r

        WiConMkPurify
         -> tLam1 kRegion $ \r -> (tConst `tApp` r) `tImpl`  (tPure  `tApp` (tRead `tApp` r))

        WiConMkShare
         -> tLam1 kRegion $ \r -> (tConst `tApp` r)  `tImpl` (tEmpty `tApp` (tFree `tApp` r))

        WiConMkDistinct n
         -> tLams (replicate n kRegion) $ \rs -> (tDistinct n) `tApps` rs

