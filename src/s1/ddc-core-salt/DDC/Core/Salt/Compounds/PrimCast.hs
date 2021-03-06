
-- | Construct applications of primitive cast operators.
module DDC.Core.Salt.Compounds.PrimCast
        ( xConvert
        , xPromote
        , xTruncate

        , typeOfPrimCast )
where
import DDC.Core.Salt.Name
import DDC.Core.Exp.Annot


-- | All the Prim Cast vars have this form.
xPrimCast a p
 = XVar a (UName (NamePrimOp $ PrimCast p))


-- | Convert a value to a similarly sized type.
xConvert :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xConvert a t1 t2 x
 = xApps a      (xPrimCast a PrimCastConvert)
                [RType t1, RType t2, RTerm x]


-- | Promote a value to a wider type.
xPromote :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xPromote a t1 t2 x
 = xApps a      (xPrimCast a PrimCastPromote)
                [RType t1, RType t2, RTerm x]


-- | Truncate a value to a narrower type.
xTruncate :: a -> Type Name -> Type Name -> Exp a Name -> Exp a Name
xTruncate a t1 t2 x
 = xApps a      (xPrimCast a PrimCastTruncate)
                [RType t1, RType t2, RTerm x]


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


