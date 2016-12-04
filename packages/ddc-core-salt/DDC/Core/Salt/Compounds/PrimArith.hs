
-- | Construct applications of primitive arithmetic operators.
module DDC.Core.Salt.Compounds.PrimArith
        ( xNeg
        , xAdd, xSub, xMul, xDiv, xMod, xRem
        , xEq,  xNeq, xLt,  xGt,  xLe,  xGe
        , xAnd, xOr
        , xShl, xShr, xBAnd, xBOr, xBXOr
        , typeOfPrimArith )
where
import DDC.Core.Salt.Compounds.PrimTyCon
import DDC.Core.Salt.Name
import DDC.Core.Exp.Annot


xNeg    = xOp1 PrimArithNeg

xAdd    = xOp2 PrimArithAdd
xSub    = xOp2 PrimArithSub
xMul    = xOp2 PrimArithMul
xDiv    = xOp2 PrimArithDiv
xMod    = xOp2 PrimArithMod
xRem    = xOp2 PrimArithRem

xEq     = xOp2 PrimArithEq
xNeq    = xOp2 PrimArithNeq
xLt     = xOp2 PrimArithLt
xGt     = xOp2 PrimArithGt
xLe     = xOp2 PrimArithLe
xGe     = xOp2 PrimArithGe

xAnd    = xOp2 PrimArithAnd
xOr     = xOp2 PrimArithOr

xShl    = xOp2 PrimArithShl
xShr    = xOp2 PrimArithShr
xBAnd   = xOp2 PrimArithBAnd
xBOr    = xOp2 PrimArithBOr
xBXOr   = xOp2 PrimArithBXOr


xOp1 :: PrimArith -> a -> Type Name -> Exp a Name -> Exp a Name
xOp1 p a tElem x1
 = let  u       = UPrim (NamePrimOp $ PrimArith $ p)
                        (typeOfPrimArith p)
   in   xApps a (XVar a u) [RType tElem, RTerm x1]


xOp2 :: PrimArith -> a -> Type Name -> Exp a Name -> Exp a Name -> Exp a Name
xOp2 p a tElem x1 x2
 = let  u       = UPrim (NamePrimOp $ PrimArith $ p)
                        (typeOfPrimArith p)
   in   xApps a (XVar a u) [RType tElem, RTerm x1, RTerm x2]


-- | Take the type of a primitive operator.
typeOfPrimArith :: PrimArith -> Type Name
typeOfPrimArith op
 = case op of
        -- Numeric
        PrimArithNeg    -> tForall kData $ \t -> t `tFun` t
        PrimArithAdd    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithSub    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithMul    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithDiv    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithMod    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithRem    -> tForall kData $ \t -> t `tFun` t `tFun` t

        -- Comparison
        PrimArithEq     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithNeq    -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithGt     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithLt     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithLe     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithGe     -> tForall kData $ \t -> t `tFun` t `tFun` tBool

        -- Boolean
        PrimArithAnd    -> tBool `tFun` tBool `tFun` tBool
        PrimArithOr     -> tBool `tFun` tBool `tFun` tBool

        -- Bitwise
        PrimArithShl    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithShr    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBAnd   -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBOr    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBXOr   -> tForall kData $ \t -> t `tFun` t `tFun` t


