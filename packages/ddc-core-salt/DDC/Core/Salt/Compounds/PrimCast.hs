
module DDC.Core.Salt.Compounds.PrimCast
        ( xConvert
        , xPromote
        , xTruncate

        , typeOfPrimCast )
where
import DDC.Core.Salt.Name
import DDC.Core.Compounds
import DDC.Core.Exp


-- | All the Prim Cast vars have this form.
xPrimCast a p
 = XVar a (UPrim (NamePrimOp $ PrimCast p) (typeOfPrimCast p))


-- | Convert a value to a similarly sized type.
xConvert :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xConvert a t1 t2 x
 = xApps a      (xPrimCast a PrimCastConvert)
                [XType a t1, XType a t2, x]


-- | Promote a value to a wider type.
xPromote :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xPromote a t1 t2 x
 = xApps a      (xPrimCast a PrimCastConvert)
                [XType a t1, XType a t2, x]


-- | Truncate a value to a narrower type.
xTruncate :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xTruncate a t1 t2 x
 = xApps a      (xPrimCast a PrimCastTruncate)
                [XType a t1, XType a t2, x]


-- | Take the type of a primitive cast.
typeOfPrimCast :: PrimCast -> Type Name
typeOfPrimCast cc
 = case cc of
        PrimCastConvert
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFun` t1

        PrimCastPromote
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFun` t1

        PrimCastTruncate
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFun` t1


