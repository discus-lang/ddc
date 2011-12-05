
module DDCI.Core.Prim.Base
        ( Prim          (..)
        , PrimOp        (..)
        , Loc           (..)
        , Rgn           (..)
        , makePrimLit
        , makePrimExp)
where
import DDCI.Core.Prim.Name
import DDC.Base.Pretty
import DDC.Base.Literal


data Prim
        = PLoc    Loc
        | PRgn    Rgn
        | PInt    Integer
        | PPrimOp PrimOp
        deriving (Eq, Show)

-- | A primitive operator.
data PrimOp
        = OpNeg
        | OpAdd
        | OpSub
        deriving (Eq, Show)


-- | A store location with its region handle.
data Loc
        = Loc Int
        deriving (Eq, Ord, Show)


-- | Region handles describe what region a store binding is in.
data Rgn
        = Rgn Int
        deriving (Eq, Ord, Show)


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

