
module DDC.Core.Simplifier.Base
        ( Simplifier(..)
        , Transform(..))
where
import DDC.Base.Pretty
import Data.Monoid


-- Simplifier -----------------------------------------------------------------
-- | Desription of how to simplify a core program
data Simplifier
        = Seq   Simplifier Simplifier
        | Trans Transform
        deriving (Eq, Show)

instance Monoid Simplifier where
 mempty  = Trans Id
 mappend = Seq


-- Transform ------------------------------------------------------------------
-- | Represents individual transforms to apply during simplification.
--   TODO: turn application of rewrite rules into its own transform.
data Transform
        = Id
        | Anonymize
        | Snip
        | Flatten
        | Beta
        | Rewrite
        | Namify
        deriving (Eq, Show)


instance Pretty Simplifier where
 ppr ss
  = case ss of
        Seq s1 s2
         -> ppr s1 <+> semi <+> ppr s2

        Trans t1
         -> ppr t1


instance Pretty Transform where
 ppr ss
  = case ss of
        Id              -> text "Id"
        Anonymize       -> text "Anonymize"
        Snip            -> text "Snip"
        Flatten         -> text "Flatten"
        Beta            -> text "Beta"
        Rewrite         -> text "Rewrite"
        Namify          -> text "Namify"
