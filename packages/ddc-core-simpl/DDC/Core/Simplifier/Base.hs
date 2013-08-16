
module DDC.Core.Simplifier.Base
        ( -- * Simplifier Specifications
          Simplifier(..)

          -- * Transform Specifications
        , Transform(..)
        , InlinerTemplates
        , NamedRewriteRules

          -- * Transform Results
        , TransformResult(..)
        , TransformInfo(..)
        , NoInformation
        , resultDone)
where
import DDC.Core.Simplifier.Result
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Transform.Namify
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Base.Pretty
import qualified DDC.Core.Transform.Snip        as Snip
import qualified DDC.Core.Transform.Eta         as Eta
import qualified DDC.Core.Transform.Beta        as Beta
import Data.Monoid


-- Simplifier -----------------------------------------------------------------
-- | Specification of how to simplify a core program.
data Simplifier s a n
        -- | Apply a single transform.
        = Trans (Transform s a n)

        -- | Apply two simplifiers in sequence.
        | Seq   (Simplifier s a n) (Simplifier s a n)

        -- | Keep applying a transform until it reports that further
        --   applications won't be helpful, bailing out after a maximum number
        --   of applications.
        | Fix   Int                (Simplifier s a n)


instance Monoid (Simplifier s a n) where
 mempty  = Trans Id
 mappend = Seq


instance Pretty (Simplifier s a n) where
 ppr ss
  = case ss of
        Seq s1 s2
         -> ppr s1 <+> text ";" <+> ppr s2

        Fix i s
         -> text "fix" <+> int i <+> ppr s

        Trans t1
         -> ppr t1


-- Transform ------------------------------------------------------------------
-- | Individual transforms to apply during simplification.
data Transform s a n
        -- | The Identity transform returns the original program unharmed.
        = Id

        -- | Rewrite named binders to anonymous deBruijn binders.
        | Anonymize

        -- | Introduce let-bindings for nested applications.
        | Snip  Snip.Config

        -- | Flatten nested let and case expressions.
        | Flatten

        -- | Perform beta reduction when the argument is not a redex.
        | Beta  Beta.Config

        -- | Perform eta expansion and reduction.
        | Eta    Eta.Config

        -- | Remove unused, pure let bindings.
        | Prune

        -- | Float single-use bindings forward into their use sites.
        | Forward

        -- | Float casts outwards.
        | Bubble

        -- | Elaborate possible Const and Distinct witnesses that aren't
        --   otherwise in the program.
        | Elaborate

        -- | Inline definitions into their use sites.
        | Inline
                { -- | Get the unfolding for a named variable.
                  transInlineDef   :: InlinerTemplates a n }

        -- | Apply general rule-based rewrites.
        | Rewrite
                { -- | List of rewrite rules along with their names.
                  transRules       :: NamedRewriteRules a n }

        -- | Rewrite anonymous binders to fresh named binders.
        | Namify
                { -- | Create a namifier to make fresh type (level-1) 
                  --   names that don't conflict with any already in this
                  --   environment.
                  transMkNamifierT :: Env n -> Namifier s n

                  -- | Create a namifier to make fresh value or witness (level-0) 
                  --   names that don't conflict with any already in this
                  --   environment.
                , transMkNamifierX :: Env n -> Namifier s n }


-- | Function to get the inliner template (unfolding) for the given name.
type InlinerTemplates a n 
        = (n -> Maybe (Exp a n))

-- | Rewrite rules along with their names.
type NamedRewriteRules a n
        = [(String, RewriteRule a n)]


instance Pretty (Transform s a n) where
 ppr ss
  = case ss of
        Id              -> text "Id"
        Anonymize       -> text "Anonymize"
        Snip{}          -> text "Snip"
        Flatten         -> text "Flatten"
        Beta{}          -> text "Beta"
        Eta{}           -> text "Eta"
        Prune           -> text "Prune"
        Forward         -> text "Forward"
        Bubble          -> text "Bubble"
        Inline{}        -> text "Inline"
        Namify{}        -> text "Namify"
        Rewrite{}       -> text "Rewrite"
        Elaborate       -> text "Elaborate"

