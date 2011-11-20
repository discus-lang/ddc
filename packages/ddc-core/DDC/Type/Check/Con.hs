
module DDC.Type.Check.Con
        ( sortOfKiCon
        , kindOfTyCon)
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
        TyConUser _ k   -> k
        TyConBuiltin tb -> kindOfTyConBuiltin tb


-- | Take the kind of a builtin type constructor.
kindOfTyConBuiltin :: TyConBuiltin -> Kind n
kindOfTyConBuiltin tb
 = case tb of
        TyConFun        -> [kData, kData, kEffect, kClosure] `kFuns` kData
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


