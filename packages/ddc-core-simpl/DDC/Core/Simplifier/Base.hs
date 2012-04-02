
module DDC.Core.Simplifier.Base
        ( Simplifier(..)
        , Transform(..))
where
import DDC.Base.Pretty
import Data.Monoid


-- Simplifier -----------------------------------------------------------------
-- | Desription of how to simplify a core program
data Simplifier
        = SimplifierSeq      Simplifier Simplifier
        | SimplifierTrans    Transform
        deriving (Eq, Show)

instance Monoid Simplifier where
 mempty  = SimplifierTrans TransformId
 mappend = SimplifierSeq


-- Transform ------------------------------------------------------------------
-- | Represents individual transforms to apply during simplification.
--   TODO: turn application of rewrite rules into its own transform.
data Transform
        = TransformId
        | TransformAnonymize
        | TransformANormal
        | TransformFlatten
        | TransformBeta
        | TransformRewrite
        deriving (Eq, Show)


instance Pretty Simplifier where
 ppr ss
  = case ss of
        SimplifierSeq s1 s2
         -> ppr s1 <+> semi <+> ppr s2

        SimplifierTrans t1
         -> ppr t1


instance Pretty Transform where
 ppr ss
  = case ss of
        TransformId             -> text "None"
        TransformAnonymize      -> text "Anonymize"
        TransformANormal        -> text "ANormal"
        TransformFlatten        -> text "Flatten"
        TransformBeta           -> text "Beta"
        TransformRewrite        -> text "Rewrite"
