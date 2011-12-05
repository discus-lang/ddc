
module DDCI.Core.Prim.Base
        ( Prim          (..)
        , PrimOp        (..)
        , makePrimLit
        , makePrimExp)
where
import DDCI.Core.Prim.Name
import DDC.Base.Pretty
import DDC.Base.Literal


data Prim
        = PInt    Integer
        | PPrimOp PrimOp
        deriving (Eq, Show)
        
data PrimOp
        = OpNeg
        | OpAdd
        | OpSub
        deriving (Eq, Show)


instance Pretty Prim where
 ppr pp
  = case pp of
        PInt i          -> text (show i)
        PPrimOp op      -> ppr op
        

instance Pretty PrimOp where
 ppr op
  = case op of
        OpNeg           -> text "neg"
        OpAdd           -> text "add"
        OpSub           -> text "sub"


-- Parsing ----------------------------------------------------------------------------------------
makePrimLit :: Literal  -> Maybe Prim
makePrimLit ll
 = case ll of
        LInteger i      -> Just $ PInt i
        _               -> Nothing

makePrimExp :: Name     -> Maybe Prim
makePrimExp (Name n)
 = case n of
        "neg"           -> Just $ PPrimOp OpNeg
        "add"           -> Just $ PPrimOp OpAdd
        "sub"           -> Just $ PPrimOp OpSub
        _               -> Nothing

