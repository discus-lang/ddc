
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
        -- Arithmetic ---------------------------
        -- | Negate elements of a vector.
        = PrimVecNeg    
        { primVecMulti          :: Int }

        -- | Add elements of a vector.
        | PrimVecAdd         
        { primVecMulti          :: Int }

        -- | Subtract elements of a vector.
        | PrimVecSub         
        { primVecMulti          :: Int }

        -- | Multiply elements of a vector.
        | PrimVecMul         
        { primVecMulti          :: Int }

        -- | Divide elements of a vector.
        | PrimVecDiv         
        { primVecMulti          :: Int }

        -- Constructors -------------------------
        -- | Replicate a scalar into a vector.
        | PrimVecRep         
        { primVecMulti          :: Int }

        -- | Pack multiple scalars into a vector
        | PrimVecPack        
        { primVecMulti          :: Int }

        -- Projections --------------------------
        -- | Extract a single element from a vector.
        | PrimVecProj
        { primVecMulti          :: Int 
        , primVecIndex          :: Int }

        -- Memory Access ------------------------
        -- | Read multiple elements  from memory.
        | PrimVecGather      
        { primVecMulti          :: Int }

        -- | Write multiple elements to   memory.
        | PrimVecScatter     
        { primVecMulti          :: Int }
        deriving (Eq, Ord, Show)


instance NFData PrimVec


instance Pretty PrimVec where
 ppr op
  = case op of
        PrimVecNeg      n       -> text "vneg$"         <> int n <> text "#"
        PrimVecAdd      n       -> text "vadd$"         <> int n <> text "#"
        PrimVecSub      n       -> text "vsub$"         <> int n <> text "#"
        PrimVecMul      n       -> text "vmul$"         <> int n <> text "#"
        PrimVecDiv      n       -> text "vdiv$"         <> int n <> text "#"

        PrimVecRep      n       -> text "vrep$"         <> int n <> text "#"
        PrimVecPack     n       -> text "vpack$"        <> int n <> text "#"
        PrimVecProj     n ix    -> text "vproj$"        <> int n <> text "$" <> int ix <> text "#"

        PrimVecGather  n        -> text "vgather$"      <> int n <> text "#"
        PrimVecScatter n        -> text "vscatter$"     <> int n <> text "#"


-- | Read a primitive vector operator.
readPrimVec :: String -> Maybe PrimVec
readPrimVec str
        | Just op <- readvop1 "vneg"     PrimVecNeg      = Just op
        | Just op <- readvop1 "vadd"     PrimVecAdd      = Just op
        | Just op <- readvop1 "vsub"     PrimVecSub      = Just op
        | Just op <- readvop1 "vmul"     PrimVecMul      = Just op
        | Just op <- readvop1 "vdiv"     PrimVecDiv      = Just op
        | Just op <- readvop1 "vrep"     PrimVecRep      = Just op
        | Just op <- readvop1 "vpack"    PrimVecPack     = Just op
        | Just op <- readvop2 "vproj"    PrimVecProj     = Just op
        | Just op <- readvop1 "vgather"  PrimVecGather   = Just op
        | Just op <- readvop1 "vscatter" PrimVecScatter  = Just op
        | otherwise                                      = Nothing

        where   readvop1 prefix op
                 | Just rest     <- stripPrefix (prefix ++ "$") str
                 , (ds, "#")     <- span isDigit rest
                 , not $ null ds
                 , n             <- read ds
                 = Just (op n)

                 | otherwise    = Nothing

                readvop2 prefix op
                 | Just rest          <- stripPrefix (prefix ++ "$") str
                 , (ds1,  '$' : str2) <- span isDigit rest
                 , not $ null ds1
                 , n1                 <- read ds1
                 , (ds2, "#")         <- span isDigit str2
                 , not $ null ds2
                 , n2                 <- read ds2
                 = Just (op n1 n2)

                 | otherwise    = Nothing


-- | Yield the multiplicity of a vector operator.
multiOfPrimVec :: PrimVec -> Maybe Int
multiOfPrimVec pp
 = case pp of
        PrimVecNeg      n       -> Just n
        PrimVecAdd      n       -> Just n
        PrimVecSub      n       -> Just n
        PrimVecMul      n       -> Just n
        PrimVecDiv      n       -> Just n
        PrimVecRep      n       -> Just n
        PrimVecPack     n       -> Just n
        PrimVecProj     n _     -> Just n
        PrimVecGather   n       -> Just n
        PrimVecScatter  n       -> Just n


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


