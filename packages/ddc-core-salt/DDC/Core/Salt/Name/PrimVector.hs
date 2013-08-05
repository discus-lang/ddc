
module DDC.Core.Salt.Name.PrimVector
        ( PrimVector (..)
        , readPrimVector
        , multiOfPrimVector
        , liftPrimArithToVector
        , lowerPrimVectorToArith)
where
import DDC.Core.Salt.Name.PrimArith
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


-- | Primitive pure vector operators that don't access memory.
data PrimVector
        = PrimVectorNeg         Int     -- ^ Negate   elements of a vector.
        | PrimVectorAdd         Int     -- ^ Add      elements of a vector.
        | PrimVectorSub         Int     -- ^ Subtract elements of a vector.
        | PrimVectorMul         Int     -- ^ Multiply elements of a vector.
        | PrimVectorDiv         Int     -- ^ Divide   elements of a vector.

        | PrimVectorRep         Int     -- ^ Replicate scalar into a vector.
        | PrimVectorPack        Int     -- ^ Pack multiple scalars into a vector.
        | PrimVectorUnpack      Int     -- ^ Unpack a vector into scalars.
        deriving (Eq, Ord, Show)


instance NFData PrimVector


instance Pretty PrimVector where
 ppr op
  = case op of
        PrimVectorNeg    n      -> text "vneg$"         <> int n <> text "#"
        PrimVectorAdd    n      -> text "vadd$"         <> int n <> text "#"
        PrimVectorSub    n      -> text "vsub$"         <> int n <> text "#"
        PrimVectorMul    n      -> text "vmul$"         <> int n <> text "#"
        PrimVectorDiv    n      -> text "vdiv$"         <> int n <> text "#"

        PrimVectorRep    n      -> text "vrep$"         <> int n <> text "#"
        PrimVectorPack   n      -> text "vpack$"        <> int n <> text "#"
        PrimVectorUnpack n      -> text "vunpack$"      <> int n <> text "#"


-- | Read a primitive vector operator.
readPrimVector :: String -> Maybe PrimVector
readPrimVector str
        | Just op       <- readvop "vneg"   PrimVectorNeg       = Just op
        | Just op       <- readvop "vadd"   PrimVectorAdd       = Just op
        | Just op       <- readvop "vsub"   PrimVectorSub       = Just op
        | Just op       <- readvop "vmul"   PrimVectorMul       = Just op
        | Just op       <- readvop "vdiv"   PrimVectorDiv       = Just op

        | Just op       <- readvop "vrep"   PrimVectorRep       = Just op
        | Just op       <- readvop "vpack"   PrimVectorPack      = Just op
        | Just op       <- readvop "vunpack" PrimVectorUnpack    = Just op

        | otherwise
        = Nothing

        where   readvop prefix op
                 | Just rest     <- stripPrefix (prefix ++ "$") str
                 , (ds, "#")     <- span isDigit rest
                 , not $ null ds
                 , n             <- read ds
                 = Just (op n)

                 | otherwise
                 = Nothing


-- | Yield the multiplicity of a vector operator.
multiOfPrimVector :: PrimVector -> Maybe Int
multiOfPrimVector pp
 = case pp of
        PrimVectorNeg n         -> Just n
        PrimVectorAdd n         -> Just n
        PrimVectorSub n         -> Just n
        PrimVectorMul n         -> Just n
        PrimVectorDiv n         -> Just n
        PrimVectorRep n         -> Just n
        PrimVectorPack n        -> Just n
        PrimVectorUnpack n      -> Just n


-- | Yield the `PrimVector` that corresponds to a `PrimArith` of the
--   given multiplicity, if any.
liftPrimArithToVector :: Int -> PrimArith -> Maybe PrimVector
liftPrimArithToVector n pp
 = case pp of
        PrimArithNeg            -> Just $ PrimVectorNeg n
        PrimArithAdd            -> Just $ PrimVectorAdd n
        PrimArithSub            -> Just $ PrimVectorSub n
        PrimArithMul            -> Just $ PrimVectorMul n
        PrimArithDiv            -> Just $ PrimVectorDiv n
        _                       -> Nothing


-- | Yield the `PrimArith` that corresponds to a `PrimVector`, if any.
lowerPrimVectorToArith :: PrimVector -> Maybe PrimArith
lowerPrimVectorToArith pp
 = case pp of
        PrimVectorNeg{}         -> Just PrimArithNeg
        PrimVectorAdd{}         -> Just PrimArithAdd
        PrimVectorSub{}         -> Just PrimArithSub
        PrimVectorMul{}         -> Just PrimArithMul
        PrimVectorDiv{}         -> Just PrimArithDiv
        _                       -> Nothing


