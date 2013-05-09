
module DDC.Core.Flow.Prim.OpStore
        ( OpStore (..)
        , readOpStore
        , typeOpStore
        , xNew,       xRead,       xWrite
        , xNewVector, xReadVector, xWriteVector, xNewVectorR, xNewVectorN
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
        -- Assignables.
        OpStoreNew              -> text "new#"
        OpStoreRead             -> text "read#"
        OpStoreWrite            -> text "write#"

        -- Vectors.
        OpStoreNewVector        -> text "newVector#"
        OpStoreNewVectorR       -> text "newVectorR#"
        OpStoreNewVectorN       -> text "newVectorN#"
        OpStoreReadVector       -> text "readVector#"
        OpStoreWriteVector      -> text "writeVector#"

        -- Streams.
        OpStoreNext             -> text "next#"


-- | Read a baked-in store operator.
readOpStore :: String -> Maybe OpStore
readOpStore str
 = case str of
        "new#"          -> Just OpStoreNew
        "read#"         -> Just OpStoreRead
        "write#"        -> Just OpStoreWrite

        "newVector#"    -> Just OpStoreNewVector
        "newVectorR#"   -> Just OpStoreNewVectorR
        "newVectorN#"   -> Just OpStoreNewVectorN
        "readVector#"   -> Just OpStoreReadVector
        "writeVector#"  -> Just OpStoreWriteVector

        "next#"         -> Just OpStoreNext
        _               -> Nothing


-- Types ----------------------------------------------------------------------
typeOpStore :: OpStore -> Type Name
typeOpStore op
 = case op of
        -- Assignables ----------------
        -- new#        :: [a : Data]. a -> Array# a
        OpStoreNew
         -> tForall kData $ \tA -> tA `tFunPE` tRef tA

        -- read#       :: [a : Data]. Ref# a -> a
        OpStoreRead
         -> tForall kData $ \tA -> tRef tA `tFunPE` tA

        -- write#      :: [a : Data]. Ref# a -> a -> Unit
        OpStoreWrite
         -> tForall kData $ \tA -> tRef tA `tFunPE` tA `tFunPE` tUnit

        -- Arrays ---------------------
        -- newVector#   :: [a : Data]. Nat -> Vector# a
        OpStoreNewVector
         -> tForall kData $ \tA -> tNat `tFunPE` tVector tA
                
        -- newVectorR#  :: [a : Data]. [k : Rate]. Vector# a
        OpStoreNewVectorR
         -> tForalls [kData, kRate] 
         $ \[tA, _] -> tVector tA
         
        -- newVectorN#  :: [a : Data]. [k : Rate]. RateNat k -> Vector a
        OpStoreNewVectorN
         -> tForalls [kData, kRate]
         $ \[tA, tK] -> tRateNat tK `tFunPE` tVector tA
        
        -- readVector#  :: [a : Data]. Vector# a -> Nat# -> a
        OpStoreReadVector
         -> tForall kData 
         $  \tA -> tVector tA `tFunPE` tNat `tFunPE` tA

        -- writeVector# :: [a : Data]. Vector# a -> Nat# -> a -> Unit
        OpStoreWriteVector
         -> tForall kData 
         $  \tA -> tVector tA `tFunPE` tNat `tFunPE` tA `tFunPE` tUnit

        -- Streams --------------------
        -- next#  :: [a : Data]. [k : Rate]. Series# k a -> Nat# -> a
        OpStoreNext
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries tK tA `tFunPE` tNat `tFunPE` tA


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


xNewVector :: Type Name -> Exp () Name -> Exp () Name
xNewVector tElem xLen
 = xApps () (xVarOpStore OpStoreNewVector)
            [XType tElem, xLen]


xNewVectorR :: Type Name -> Type Name -> Exp () Name
xNewVectorR tElem tR
 = xApps () (xVarOpStore OpStoreNewVectorR)
            [XType tElem, XType tR]


xNewVectorN :: Type Name -> Type Name -> Exp () Name -> Exp () Name
xNewVectorN tA tR  xRN
 = xApps () (xVarOpStore OpStoreNewVectorN)
            [XType tA, XType tR, xRN]


xReadVector :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xReadVector t xArr xIx
 = xApps () (xVarOpStore OpStoreReadVector)
            [XType t, xArr, xIx]


xWriteVector :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name -> Exp () Name
xWriteVector t xArr xIx xElem
 = xApps () (xVarOpStore OpStoreWriteVector)
            [XType t, xArr, xIx, xElem]


xNext  :: Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNext tRate tElem xStream xIndex
 = xApps () (xVarOpStore OpStoreNext)
            [XType tElem, XType tRate, xStream, xIndex]


-- Utils ----------------------------------------------------------------------
xVarOpStore :: OpStore -> Exp () Name
xVarOpStore op
        = XVar () (UPrim (NameOpStore op) (typeOpStore op))





