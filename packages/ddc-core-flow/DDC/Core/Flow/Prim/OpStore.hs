
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
        -- new#   :: [a : Data]. a -> Array# a
        OpStoreNew
         -> tForall kData $ \tA -> tA `tFunPE` tRef tA

        -- read#  :: [a : Data]. Ref# a -> a
        OpStoreRead
         -> tForall kData $ \tA -> tRef tA `tFunPE` tA

        -- write# :: [a : Data]. Ref# a -> a -> Unit
        OpStoreWrite
         -> tForall kData $ \tA -> tRef tA `tFunPE` tA `tFunPE` tUnit

        -- next#  :: [k : Rate]. [a : Data]. Stream# k a -> Nat# -> a
        OpStoreNext
         -> tForalls [kRate, kData]
         $  \[tK, tA] -> tStream tK tA `tFunPE` tNat `tFunPE` tA


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


xNext  :: Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNext tRate tElem xStream xIndex
 = xApps () (xVarOpStore OpStoreNext)
            [XType tRate, XType tElem, xStream, xIndex]


-- Utils ----------------------------------------------------------------------
xVarOpStore :: OpStore -> Exp () Name
xVarOpStore op
        = XVar () (UPrim (NameOpStore op) (typeOpStore op))





