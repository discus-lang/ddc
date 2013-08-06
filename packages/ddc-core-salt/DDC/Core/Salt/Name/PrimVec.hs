
module DDC.Core.Salt.Name.PrimVec
        ( PrimVec       (..)
        , readPrimVec
        , multiOfPrimVec
        , liftPrimArithToVec
        , lowerPrimVecToArith)
where
import DDC.Core.Salt.Name.PrimArith
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


-- | Primitive vector operators.
data PrimVec
        -- | Negate elements of a vector.
        = PrimVecNeg         Int

        -- | Add elements of a vector.
        | PrimVecAdd         Int

        -- | Subtract elements of a vector.
        | PrimVecSub         Int

        -- | Multiple elements of a vector.
        | PrimVecMul         Int

        -- | Divide elements of a vector.
        | PrimVecDiv         Int

        -- | Replicate a scalar into a vector.
        | PrimVecRep         Int

        -- | Pack multiple scalars into a vector
        | PrimVecPack        Int

        -- | Unpack multiple scalars from a vector.
        | PrimVecUnpack      Int

        -- | Read multiple elements  from memory.
        | PrimVecGather      Int

        -- | Write multiple elements to   memory.
        | PrimVecScatter     Int
        deriving (Eq, Ord, Show)


instance NFData PrimVec


instance Pretty PrimVec where
 ppr op
  = case op of
        PrimVecNeg     n        -> text "vneg$"         <> int n <> text "#"
        PrimVecAdd     n        -> text "vadd$"         <> int n <> text "#"
        PrimVecSub     n        -> text "vsub$"         <> int n <> text "#"
        PrimVecMul     n        -> text "vmul$"         <> int n <> text "#"
        PrimVecDiv     n        -> text "vdiv$"         <> int n <> text "#"

        PrimVecRep     n        -> text "vrep$"         <> int n <> text "#"
        PrimVecPack    n        -> text "vpack$"        <> int n <> text "#"
        PrimVecUnpack  n        -> text "vunpack$"      <> int n <> text "#"

        PrimVecGather  n        -> text "vgather$"      <> int n <> text "#"
        PrimVecScatter n        -> text "vscatter$"     <> int n <> text "#"


-- | Read a primitive vector operator.
readPrimVec :: String -> Maybe PrimVec
readPrimVec str
        | Just op <- readvop "vneg"     PrimVecNeg      = Just op
        | Just op <- readvop "vadd"     PrimVecAdd      = Just op
        | Just op <- readvop "vsub"     PrimVecSub      = Just op
        | Just op <- readvop "vmul"     PrimVecMul      = Just op
        | Just op <- readvop "vdiv"     PrimVecDiv      = Just op
        | Just op <- readvop "vrep"     PrimVecRep      = Just op
        | Just op <- readvop "vpack"    PrimVecPack     = Just op
        | Just op <- readvop "vunpack"  PrimVecUnpack   = Just op
        | Just op <- readvop "vgather"  PrimVecGather   = Just op
        | Just op <- readvop "vscatter" PrimVecScatter  = Just op
        | otherwise                                     = Nothing

        where   readvop prefix op
                 | Just rest     <- stripPrefix (prefix ++ "$") str
                 , (ds, "#")     <- span isDigit rest
                 , not $ null ds
                 , n             <- read ds
                 = Just (op n)

                 | otherwise
                 = Nothing


-- | Yield the multiplicity of a vector operator.
multiOfPrimVec :: PrimVec -> Maybe Int
multiOfPrimVec pp
 = case pp of
        PrimVecNeg n            -> Just n
        PrimVecAdd n            -> Just n
        PrimVecSub n            -> Just n
        PrimVecMul n            -> Just n
        PrimVecDiv n            -> Just n
        PrimVecRep n            -> Just n
        PrimVecPack n           -> Just n
        PrimVecUnpack  n        -> Just n
        PrimVecGather  n        -> Just n
        PrimVecScatter n        -> Just n


-- | Yield the `PrimVector` that corresponds to a `PrimArith` of the
--   given multiplicity, if any.
liftPrimArithToVec :: Int -> PrimArith -> Maybe PrimVec
liftPrimArithToVec n pp
 = case pp of
        PrimArithNeg            -> Just $ PrimVecNeg n
        PrimArithAdd            -> Just $ PrimVecAdd n
        PrimArithSub            -> Just $ PrimVecSub n
        PrimArithMul            -> Just $ PrimVecMul n
        PrimArithDiv            -> Just $ PrimVecDiv n
        _                       -> Nothing


-- | Yield the `PrimArith` that corresponds to a `PrimVector`, if any.
lowerPrimVecToArith :: PrimVec -> Maybe PrimArith
lowerPrimVecToArith pp
 = case pp of
        PrimVecNeg{}            -> Just PrimArithNeg
        PrimVecAdd{}            -> Just PrimArithAdd
        PrimVecSub{}            -> Just PrimArithSub
        PrimVecMul{}            -> Just PrimArithMul
        PrimVecDiv{}            -> Just PrimArithDiv
        _                       -> Nothing


