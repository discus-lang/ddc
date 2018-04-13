
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
import DDC.Data.Pretty
import qualified Data.Semigroup         as SG
import qualified DDC.Core.Transform.Snip        as Snip
import qualified DDC.Core.Transform.Eta         as Eta
import qualified DDC.Core.Transform.Beta        as Beta
import qualified DDC.Core.Transform.FoldCase    as FoldCase


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


instance SG.Semigroup (Simplifier s a n) where
 (<>)           = unionSimplifier


instance Monoid (Simplifier s a n) where
 mempty         = emptySimplifier
 mappend        = unionSimplifier


instance Pretty (Simplifier s a n) where
 ppr ss
  = case ss of
        Seq s1 s2 -> ppr s1 %% text ";" %% ppr s2
        Fix i s   -> text "fix" %% int i %% ppr s
        Trans t1  -> ppr t1


-- | Construct an empty simplifier.
emptySimplifier :: Simplifier s a n
emptySimplifier = Trans Id


-- | Union two simplifiers.
unionSimplifier :: Simplifier s a n -> Simplifier s a n -> Simplifier s a n
unionSimplifier = Seq


-- Transform ------------------------------------------------------------------
-- | Individual transforms to apply during simplification.
data Transform s a n
        -- | The Identity transform returns the original program unharmed.
        = Id

        -- | Rewrite named binders to anonymous deBruijn binders.
        | Anonymize

        -- | Perform beta reduction when the argument is not a redex.
        | Beta  Beta.Config

        -- | Float casts outwards.
        | Bubble

        -- | Elaborate possible Const and Distinct witnesses that aren't
        --   otherwise in the program.
        | Elaborate

        -- | Perform eta expansion and reduction.
        | Eta    Eta.Config

        -- | Inline type equations
        --   and convert to explicit abstraction and application.
        | Expliciate

        -- | Flatten nested let and case expressions.
        | Flatten

        -- | Float single-use bindings forward into their use sites.
        | Forward

        -- | Fold case expressions.
        | FoldCase FoldCase.Config

        -- | Inline definitions into their use sites.
        | Inline
                { -- | Get the unfolding for a named variable.
                  transInlineDef   :: InlinerTemplates a n }

        -- | Perform lambda lifting.
        | Lambdas

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

        -- | Remove unused, pure let bindings.
        | Prune

        -- | Apply general rule-based rewrites.
        | Rewrite
                { -- | List of rewrite rules along with their names.
                  transRules       :: NamedRewriteRules a n }

        -- | Introduce let-bindings for nested applications.
        | Snip  Snip.Config


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
        Beta{}          -> text "Beta"
        Bubble          -> text "Bubble"
        Elaborate       -> text "Elaborate"
        Eta{}           -> text "Eta"
        Expliciate      -> text "Expliciate"
        Flatten         -> text "Flatten"
        Forward         -> text "Forward"
        FoldCase{}      -> text "FoldCase"
        Inline{}        -> text "Inline"
        Lambdas{}       -> text "Lambdas"
        Namify{}        -> text "Namify"
        Prune           -> text "Prune"
        Rewrite{}       -> text "Rewrite"
        Snip{}          -> text "Snip"


