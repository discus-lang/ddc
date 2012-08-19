
module DDC.Core.Simplifier.Base
        ( Simplifier(..)
        , Transform(..)
	, TransformInfo(..)
	, TransformResult(..)
	, NoInformation(..)
	, resultSimple)
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
-- | Desription of how to simplify a core program
data Simplifier s a n
        = Seq   (Simplifier s a n) (Simplifier s a n)
	| Fix	Int		   (Simplifier s a n)
        | Trans (Transform s a n)


instance Monoid (Simplifier s a n) where
 mempty  = Trans Id
 mappend = Seq


-- Transform ------------------------------------------------------------------
-- | Represents individual transforms to apply during simplification.
data Transform s a n
        -- | The Identity transform returns the program unharmed.
        = Id

        -- | Rewrite named binders to anonymous deBruijn binders.
        | Anonymize

        -- | Introduce let-bindings for nested applications.
        | Snip

        -- | Flatten nested let and case expressions.
        | Flatten

        -- | Perform beta reduction for atomic arguments.
        | Beta

        -- | Carry function bindings forward into their use sites.
        | Forward

        -- | Float casts outwards.
        | Bubble

        -- | Inline definitions into their use sites.
        | Inline
                { transInlineDef   :: n -> Maybe (Exp a n) }

        -- | Rewrite anonymous binders to fresh named binders.
        | Namify
                { transMkNamifierT :: Env n -> Namifier s n
                , transMkNamifierX :: Env n -> Namifier s n }

        -- | Apply general rule-based rewrites.
        | Rewrite
                { transRules       :: [(String,RewriteRule a n)] }



instance Pretty (Simplifier s a n) where
 ppr ss
  = case ss of
        Seq s1 s2
         -> ppr s1 <+> semi <+> ppr s2

        Fix i s
         -> text "fix" <+> int i <+> text "(" P.<> ppr s P.<> text ")"

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
        Forward         -> text "Forward"
        Bubble          -> text "Bubble"
        Inline{}        -> text "Inline"
        Namify{}        -> text "Namify"
        Rewrite{}       -> text "Rewrite"


instance Pretty (TransformResult r) where
 ppr (TransformResult _ _ (TransformInfo i))
  = ppr i

