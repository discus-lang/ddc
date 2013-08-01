
module DDC.Core.Flow.Prim.OpStore
        ( OpStore (..)
        , readOpStore
        , typeOpStore
        , xNew,       xRead,       xWrite
        , xNewVector, xReadVector, xWriteVector, xNewVectorR, xNewVectorN
        , xSliceVector
        , xNext)
where
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char

instance NFData OpStore


instance Pretty OpStore where
 ppr so
  = case so of
        -- Assignables.
        OpStoreNew              -> text "new#"
        OpStoreRead             -> text "read#"
        OpStoreWrite            -> text "write#"

        -- Vectors.
        OpStoreNewVector        -> text "vnew#"
        OpStoreNewVectorR       -> text "vnewR#"
        OpStoreNewVectorN       -> text "vnewN#"
        OpStoreReadVector       -> text "vread#"
        OpStoreWriteVector      -> text "vwrite#"
        OpStoreSliceVector      -> text "vslice#"

        -- Streams.
        OpStoreNext 1           -> text "next#"
        OpStoreNext n           -> text "next" <> int n <> text "#"


-- | Read a store operator name.
readOpStore :: String -> Maybe OpStore
readOpStore str
        | Just rest     <- stripPrefix "next" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpStoreNext n

        | otherwise
        = case str of
                "new#"          -> Just OpStoreNew
                "read#"         -> Just OpStoreRead
                "write#"        -> Just OpStoreWrite
        
                "vnew#"         -> Just OpStoreNewVector
                "vnewR#"        -> Just OpStoreNewVectorR
                "vnewN#"        -> Just OpStoreNewVectorN
                "vread#"        -> Just OpStoreReadVector
                "vwrite#"       -> Just OpStoreWriteVector
                "vslice#"       -> Just OpStoreSliceVector

                "next#"         -> Just (OpStoreNext 1)
                _               -> Nothing


-- Types ----------------------------------------------------------------------
-- | Yield the type of a store operator.
typeOpStore :: OpStore -> Type Name
typeOpStore op
 = case op of
        -- Assignables ----------------
        -- new#        :: [a : Data]. a -> Array# a
        OpStoreNew
         -> tForall kData $ \tA -> tA `tFun` tRef tA

        -- read#       :: [a : Data]. Ref# a -> a
        OpStoreRead
         -> tForall kData $ \tA -> tRef tA `tFun` tA

        -- write#      :: [a : Data]. Ref# a -> a -> Unit
        OpStoreWrite
         -> tForall kData $ \tA -> tRef tA `tFun` tA `tFun` tUnit

        -- Arrays ---------------------
        -- vnew#   :: [a : Data]. Nat -> Vector# a
        OpStoreNewVector
         -> tForall kData $ \tA -> tNat `tFun` tVector tA
                
        -- vnew#  :: [a : Data]. [k : Rate]. Vector# a
        OpStoreNewVectorR
         -> tForalls [kData, kRate] 
         $ \[tA, _] -> tVector tA
         
        -- vnew#  :: [a : Data]. [k : Rate]. RateNat k -> Vector a
        OpStoreNewVectorN
         -> tForalls [kData, kRate]
         $ \[tA, tK] -> tRateNat tK `tFun` tVector tA
        
        -- vread#  :: [a : Data]. Vector# a -> Nat# -> a
        OpStoreReadVector
         -> tForall kData 
         $  \tA -> tVector tA `tFun` tNat `tFun` tA

        -- vwrite# :: [a : Data]. Vector# a -> Nat# -> a -> Unit
        OpStoreWriteVector
         -> tForall kData 
         $  \tA -> tVector tA `tFun` tNat `tFun` tA `tFun` tUnit

        -- vslice# :: [a : Data]. Nat# -> Vector# a -> Vector# a
        OpStoreSliceVector
         -> tForall kData 
         $  \tA -> tNat `tFun` tVector tA `tFun` tVector tA


        -- Streams --------------------
        -- next#  :: [a : Data]. [k : Rate]. Series# k a -> Nat# -> a
        OpStoreNext 1
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries tK tA `tFun` tNat `tFun` tA

        -- nextN# :: [a : Data]. [k : Rate]. Series# (DownN# k) a -> Nat# -> VecN# a
        OpStoreNext n
         -> tForalls [kData, kRate]
         $  \[tA, tK] -> tSeries (tDown n tK) tA `tFun` tNat `tFun` tVec n tA


-- Compounds ------------------------------------------------------------------
xNew :: Type Name -> Exp () Name -> Exp () Name
xNew t xV
 = xApps (xVarOpStore OpStoreNew)
         [XType t, xV ]


xRead :: Type Name -> Exp () Name -> Exp () Name
xRead t xRef
 = xApps (xVarOpStore OpStoreRead)
         [XType t, xRef ]


xWrite :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xWrite t xRef xVal
 = xApps (xVarOpStore OpStoreWrite)
         [XType t, xRef, xVal ]


xNewVector :: Type Name -> Exp () Name -> Exp () Name
xNewVector tElem xLen
 = xApps (xVarOpStore OpStoreNewVector)
         [XType tElem, xLen]


xNewVectorR :: Type Name -> Type Name -> Exp () Name
xNewVectorR tElem tR
 = xApps (xVarOpStore OpStoreNewVectorR)
         [XType tElem, XType tR]


xNewVectorN :: Type Name -> Type Name -> Exp () Name -> Exp () Name
xNewVectorN tA tR  xRN
 = xApps (xVarOpStore OpStoreNewVectorN)
         [XType tA, XType tR, xRN]


xReadVector :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xReadVector t xArr xIx
 = xApps (xVarOpStore OpStoreReadVector)
         [XType t, xArr, xIx]


xWriteVector :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name -> Exp () Name
xWriteVector t xArr xIx xElem
 = xApps (xVarOpStore OpStoreWriteVector)
         [XType t, xArr, xIx, xElem]

xSliceVector :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xSliceVector tElem xLen xArr
 = xApps (xVarOpStore OpStoreSliceVector)
         [XType tElem, xLen, xArr]


xNext  :: Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xNext tRate tElem xStream xIndex
 = xApps (xVarOpStore (OpStoreNext 1))
         [XType tElem, XType tRate, xStream, xIndex]


-- Utils ----------------------------------------------------------------------
xVarOpStore :: OpStore -> Exp () Name
xVarOpStore op
        = XVar (UPrim (NameOpStore op) (typeOpStore op))

