
module DDC.Core.Simplifier.Base
        ( Simplifier(..)
        , Transform(..))
where
import DDC.Core.Transform.Rewrite
import DDC.Core.Transform.Namify
import DDC.Type.Env
import DDC.Base.Pretty
import Data.Monoid


-- Simplifier -----------------------------------------------------------------
-- | Desription of how to simplify a core program
data Simplifier s a n
        = Seq   (Simplifier s a n) (Simplifier s a n)
        | Trans (Transform s a n)


instance Monoid (Simplifier s a n) where
 mempty  = Trans Id
 mappend = Seq


-- Transform ------------------------------------------------------------------
-- | Represents individual transforms to apply during simplification.
---
--   TODO: turn application of rewrite rules into its own transform.
data Transform s a n
        = Id
        | Anonymize
        | Snip
        | Flatten
        | Beta
        | Rewrite       [RewriteRule a n]
        | Namify        (Env n -> Namifier s n) 
                        (Env n -> Namifier s n)


instance Pretty (Simplifier s a n) where
 ppr ss
  = case ss of
        Seq s1 s2
         -> ppr s1 <+> semi <+> ppr s2

        Trans t1
         -> ppr t1


instance Pretty (Transform s a n) where
 ppr ss
  = case ss of
        Id              -> text "Id"
        Anonymize       -> text "Anonymize"
        Snip            -> text "Snip"
        Flatten         -> text "Flatten"
        Beta            -> text "Beta"
        Rewrite{}       -> text "Rewrite"
        Namify{}        -> text "Namify"
