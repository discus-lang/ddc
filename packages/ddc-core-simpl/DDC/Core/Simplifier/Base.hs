
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
	, NoInformation(..)
	, resultDone)
where
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Transform.Namify
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Base.Pretty
import qualified DDC.Base.Pretty	as P
import Data.Monoid
import Data.Typeable (Typeable)


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
	| Fix	Int		   (Simplifier s a n)


instance Monoid (Simplifier s a n) where
 mempty  = Trans Id
 mappend = Seq


instance Pretty (Simplifier s a n) where
 ppr ss
  = case ss of
        Seq s1 s2
         -> ppr s1 <+> text "<>" <+> ppr s2

        Fix i s
         -> text "fix" <+> int i <+> text "(" P.<> ppr s P.<> text ")"

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
        | Snip

        -- | Flatten nested let and case expressions.
        | Flatten

        -- | Perform beta reduction when the argument is not a redex.
        | Beta

        -- | Perform beta reduction, introducing new let-bindings for 
        --   arguments that are redexes.
        | BetaLets

        -- | Remove unused, pure let bindings.
        | DeadCode

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
        Snip            -> text "Snip"
        Flatten         -> text "Flatten"
        Beta            -> text "Beta"
        BetaLets        -> text "BetaLets"
        DeadCode        -> text "DeadCode"
        Forward         -> text "Forward"
        Bubble          -> text "Bubble"
        Inline{}        -> text "Inline"
        Namify{}        -> text "Namify"
        Rewrite{}       -> text "Rewrite"
        Elaborate       -> text "Elaborate"


-- TransformResult ------------------------------------------------------------
-- | Package up the result of applying a single transform.
data TransformResult r
        = TransformResult
        { -- | Transform result proper (eg the new module)
          result         :: r

          -- | Whether this transform made any progess.
          --   
          --   If `False` then the result program must be the same as the
          --   input program, and a simplifer fixpoint won't apply this
          --   transform again to the result program.
        , resultProgress :: Bool

          -- | Whether it might help to run the same transform again.
          -- 
          --   If `False` then a simplifier fixpoint won't apply this transform
          --   again to the result program.
        , resultAgain    :: Bool

          -- | Transform specific log. This might contain a count of what rules
          --   fired, or information about what parts of the program couldn't
          --   be processed.
        , resultInfo     :: TransformInfo }


-- | Existential package for a typeable thing,
--   used in `TransformResult`.
data TransformInfo
	=  forall i
        .  (Typeable i, Pretty i)
	=> TransformInfo i


-- | Place-holder type to use when there is no real `TransformResult`.
data NoInformation 
        = NoInformation String
        deriving Typeable


instance Pretty NoInformation where
    ppr (NoInformation name) = text name P.<> text ": No information"


instance Pretty (TransformResult r) where
 ppr (TransformResult _ _ _ (TransformInfo i))
  = ppr i


-- | Create a default result with no transform again.
--  
--   We'll say we made progress, but set `resultAgain` to False
--   so to stop any simplifier fixpoints.
resultDone :: String -> r -> TransformResult r
resultDone name r 
        = TransformResult r True False
        $ TransformInfo 
        $ NoInformation name
