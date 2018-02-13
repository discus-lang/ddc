
module DDC.Core.Discus.Prim.OpArith
        ( readPrimArithFlag
        , typePrimArithFlag)
where
import DDC.Core.Discus.Prim.TyConDiscus
import DDC.Core.Discus.Prim.TyConPrim
import DDC.Core.Discus.Prim.Base
import DDC.Type.Exp.Simple
import Data.List


-- | Take the type of a primitive arithmetic operator.
typePrimArithFlag :: PrimArith -> Bool -> Type Name
typePrimArithFlag op bUnboxed
 = let
        fb | bUnboxed   = tUnboxed
           | otherwise  = id

        tOp1            = tForall kData $ \t -> fb t `tFun` fb t
        tOp2            = tForall kData $ \t -> fb t `tFun` fb t `tFun` fb t
        tEq             = tForall kData $ \t -> fb t `tFun` fb t `tFun` fb tBool

   in case op of
        PrimArithNeg    -> tOp1

        PrimArithAdd    -> tOp2
        PrimArithSub    -> tOp2
        PrimArithMul    -> tOp2
        PrimArithDiv    -> tOp2
        PrimArithMod    -> tOp2
        PrimArithRem    -> tOp2
        PrimArithShl    -> tOp2
        PrimArithShr    -> tOp2
        PrimArithBAnd   -> tOp2
        PrimArithBOr    -> tOp2
        PrimArithBXOr   -> tOp2
        PrimArithAnd    -> tOp2
        PrimArithOr     -> tOp2

        PrimArithEq     -> tEq
        PrimArithNeq    -> tEq
        PrimArithGt     -> tEq
        PrimArithLt     -> tEq
        PrimArithLe     -> tEq
        PrimArithGe     -> tEq



-- | Read a primitive operator.
readPrimArithFlag :: String -> Maybe (PrimArith, Bool)
readPrimArithFlag str
  =  case find (\(_, n) -> str == n) primArithNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
primArithNames :: [((PrimArith, Bool), String)]
primArithNames
 = concat
        $ [ [ ((p, False),  str)
            , ((p, True),   str ++ "#")]
          | (p, str) <- table]
 where
  table
   =    [ (PrimArithNeg,        "neg#")
        , (PrimArithAdd,        "add#")
        , (PrimArithSub,        "sub#")
        , (PrimArithMul,        "mul#")
        , (PrimArithDiv,        "div#")
        , (PrimArithRem,        "rem#")
        , (PrimArithMod,        "mod#")
        , (PrimArithEq,         "eq#" )
        , (PrimArithNeq,        "neq#")
        , (PrimArithGt,         "gt#" )
        , (PrimArithGe,         "ge#" )
        , (PrimArithLt,         "lt#" )
        , (PrimArithLe,         "le#" )
        , (PrimArithAnd,        "and#")
        , (PrimArithOr,         "or#" )
        , (PrimArithShl,        "shl#")
        , (PrimArithShr,        "shr#")
        , (PrimArithBAnd,       "band#")
        , (PrimArithBOr,        "bor#")
        , (PrimArithBXOr,       "bxor#") ]
