
module DDC.Core.Flow.Prim.OpStore
        ( OpStore (..)
        , readOpStore
        , typeOpStore
        , xNew,         xRead,       xWrite
        , xNewVector,   xNewVectorR, xNewVectorN
        , xReadVector,  xReadVectorC
        , xWriteVector, xWriteVectorC
        , xTailVector
        , xTruncVector
        , xBufOfVector, xBufOfRateVec)
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

        OpStoreReadVector  1    -> text "vread#"
        OpStoreReadVector  n    -> text "vread$"  <> int n <> text "#"

        OpStoreWriteVector 1    -> text "vwrite#"
        OpStoreWriteVector n    -> text "vwrite$" <> int n <> text "#"

        OpStoreTailVector  1    -> text "vtail#"
        OpStoreTailVector  n    -> text "vtail"   <> int n <> text "#"

        OpStoreTruncVector      -> text "vtrunc#"
        OpStoreBufOfVector      -> text "vbuf#"
        OpStoreBufOfRateVec     -> text "vbufofratevec#"


-- | Read a store operator name.
readOpStore :: String -> Maybe OpStore
readOpStore str
        | Just rest     <- stripPrefix "vread$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpStoreReadVector n

        | Just rest     <- stripPrefix "vwrite$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpStoreWriteVector n

        | Just rest     <- stripPrefix "vtail$" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 1
        = Just $ OpStoreTailVector n

        | otherwise
        = case str of
                "new#"          -> Just OpStoreNew
                "read#"         -> Just OpStoreRead
                "write#"        -> Just OpStoreWrite
        
                "vnew#"         -> Just OpStoreNewVector
                "vnewR#"        -> Just OpStoreNewVectorR
                "vnewN#"        -> Just OpStoreNewVectorN
                "vread#"        -> Just (OpStoreReadVector  1)
                "vwrite#"       -> Just (OpStoreWriteVector 1)
                "vtail#"        -> Just (OpStoreTailVector  1)
                "vtrunc#"       -> Just OpStoreTruncVector
                "vbuf#"         -> Just OpStoreBufOfVector
                "vbufofratevec#"-> Just OpStoreBufOfRateVec

                _               -> Nothing


-- Types ----------------------------------------------------------------------
-- | Yield the type of a store operator.
typeOpStore :: OpStore -> Type Name
typeOpStore op
 = case op of
        -- Assignables ----------------
        -- new#        :: [a : Data]. a -> Ref# a
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
        OpStoreReadVector 1
         -> tForall kData 
         $  \tA -> tBuffer tA `tFun` tNat `tFun` tA

        -- vreadN#  :: [a : Data]. Vector# a -> Nat# -> VecN# a
        OpStoreReadVector n
         -> tForall kData 
         $  \tA -> tBuffer tA `tFun` tNat `tFun` tVec n tA

        -- vwrite# :: [a : Data]. Vector# a -> Nat# -> a -> Unit
        OpStoreWriteVector 1
         -> tForall kData 
         $  \tA -> tBuffer tA `tFun` tNat `tFun` tA `tFun` tUnit

        -- vwriteN# :: [a : Data]. Vector# a -> Nat# -> VecN# a -> Unit
        OpStoreWriteVector n
         -> tForall kData 
         $  \tA -> tBuffer tA `tFun` tNat `tFun` tVec n tA `tFun` tUnit

        -- vtail$N# :: [k : Rate]. [a : Data]. RateNat (TailN k) -> Vector# a -> Vector# a
        OpStoreTailVector n
         -> tForalls [kRate, kData]
         $  \[tK, tA] -> tRateNat (tTail n tK) `tFun` tVector tA `tFun` tVector tA

        -- vtrunc#  :: [a : Data]. Nat# -> Vector# a -> Unit
        OpStoreTruncVector
         -> tForall kData 
         $  \tA -> tNat `tFun` tVector tA `tFun` tUnit

        -- vbuf#   :: [a : Data]. Vector# a -> Buffer# a
        OpStoreBufOfVector
         -> tForall kData 
         $  \tA -> tVector tA `tFun` tBuffer tA

        -- vbufofratevec#   :: [k : Rate]. [a : Data]. RateVec# k a -> Buffer# a
        OpStoreBufOfRateVec
         -> tForalls [kRate, kData]
         $  \[tK, tA] -> tRateVec tK tA `tFun` tBuffer tA



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
 = xApps (xVarOpStore (OpStoreReadVector 1))
         [XType t, xArr, xIx]


xReadVectorC :: Int -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xReadVectorC c t xArr xIx
 = xApps (xVarOpStore (OpStoreReadVector c))
         [XType t, xArr, xIx]


xWriteVector :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name -> Exp () Name
xWriteVector t xArr xIx xElem
 = xApps (xVarOpStore (OpStoreWriteVector 1))
         [XType t, xArr, xIx, xElem]


xWriteVectorC :: Int -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name -> Exp () Name
xWriteVectorC c t xArr xIx xElem
 = xApps (xVarOpStore (OpStoreWriteVector c))
         [XType t, xArr, xIx, xElem]


xTailVector :: Int -> Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xTailVector n tK tA xRN xVec
 = xApps (xVarOpStore (OpStoreTailVector n))
         [XType tK, XType tA, xRN, xVec]


xTruncVector :: Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xTruncVector tElem xLen xArr
 = xApps (xVarOpStore OpStoreTruncVector)
         [XType tElem, xLen, xArr]

xBufOfVector :: Type Name -> Exp () Name -> Exp () Name
xBufOfVector tElem xArr
 = xApps (xVarOpStore OpStoreBufOfVector)
         [XType tElem, xArr]

xBufOfRateVec :: Type Name -> Type Name -> Exp () Name -> Exp () Name
xBufOfRateVec tRate tElem xArr
 = xApps (xVarOpStore OpStoreBufOfRateVec)
         [XType tRate, XType tElem, xArr]




-- Utils ----------------------------------------------------------------------
xVarOpStore :: OpStore -> Exp () Name
xVarOpStore op
        = XVar (UPrim (NameOpStore op) (typeOpStore op))

