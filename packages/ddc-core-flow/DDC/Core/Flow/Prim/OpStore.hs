
module DDC.Core.Flow.Prim.OpStore
        ( OpStore (..)
        , readOpStore
        , typeOpStore
        , xNew
        , xRead
        , xWrite
        , xNext)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpStore


instance Pretty OpStore where
 ppr so
  = case so of
        OpStoreNew              -> text "new#"
        OpStoreRead             -> text "read#"
        OpStoreWrite            -> text "write#"

        OpStoreNext             -> text "next#"


-- | Read a baked-in store operator.
readOpStore :: String -> Maybe OpStore
readOpStore str
 = case str of
        "new#"          -> Just OpStoreNew
        "read#"         -> Just OpStoreRead
        "write#"        -> Just OpStoreWrite

        "next#"         -> Just OpStoreNext
        _               -> Nothing


-- Types ----------------------------------------------------------------------
typeOpStore :: OpStore -> Type Name
typeOpStore op
 = case op of
        -- new#   :: [a : Data]. Int -> Array a
        OpStoreNew
         -> tForall kData $ \tA -> tInt `tFunPE` tArray tA

        -- read#  :: [a : Data]. Array a -> Int -> a
        OpStoreRead
         -> tForall kData $ \tA -> tArray tA `tFunPE` tInt `tFunPE` tA

        -- write# :: [a : Data]. Array a -> Int -> a -> Unit
        OpStoreWrite
         -> tForall kData $ \tA -> tArray tA `tFunPE` tInt `tFunPE` tA `tFunPE` tUnit

        -- next#  :: [k : Rate]. [a : Data]. Stream k a -> Int -> a
        OpStoreNext
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tStream tK tA `tFunPE` tInt `tFunPE` tA


-- Compounds ------------------------------------------------------------------
xNew :: Type Name -> Exp () Name -> Exp () Name
xNew t xV
 = xApps () (xVarOpStore OpStoreNew)
            [XType t, xV ]


xRead :: Type Name -> Exp () Name -> Exp () Name
xRead t xRef
 = xApps () (xVarOpStore OpStoreRead)
            [XType t, xRef ]


xWrite :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xWrite t xRef xVal
 = xApps () (xVarOpStore OpStoreWrite)
            [XType t, xRef, xVal ]


xNext  :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNext t xStream xIndex
 = xApps () (xVarOpStore OpStoreNext)
            [XType t, xStream, xIndex]


-- Utils ----------------------------------------------------------------------
xVarOpStore :: OpStore -> Exp () Name
xVarOpStore op
        = XVar () (UPrim (NameOpStore op) (typeOpStore op))





