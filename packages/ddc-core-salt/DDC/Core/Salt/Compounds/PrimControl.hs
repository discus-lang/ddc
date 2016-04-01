
-- | Construct applications of primitive control operators.
module DDC.Core.Salt.Compounds.PrimControl
        ( xFail
        , xReturn
        , typeOfPrimControl)
where
import DDC.Core.Salt.Name
import DDC.Core.Compounds
import DDC.Core.Exp


-- | All the Prim Control vars have this form.
xPrimControl a p
 = XVar a (UPrim (NamePrimOp $ PrimControl p) (typeOfPrimControl p))


-- | Fail with an internal error.
xFail   :: a -> Type Name -> Exp a Name
xFail a t
 = xApps a      (xPrimControl a PrimControlFail)
                [XType a t]


-- | Return a value.
xReturn :: a -> Type Name -> Exp a Name -> Exp a Name
xReturn a t x
 = xApps a      (xPrimControl a PrimControlReturn)
                [XType a t, x]


typeOfPrimControl :: PrimControl -> Type Name
typeOfPrimControl pc
 = case pc of
        PrimControlFail         -> tForall kData $ \t -> t
        PrimControlReturn       -> tForall kData $ \t -> t `tFun` t


