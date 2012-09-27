
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
        -- | Apply two simplifiers in sequence.
        = Seq   (Simplifier s a n) (Simplifier s a n)

        -- | Apply a transform until it stops progressing,
        --   or bail out after a maximum number of applications.
	| Fix	Int		   (Simplifier s a n)

        -- | Apply a single transform.
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

        -- | Perform beta reduction, turning non-atomic arguments into lets.
        | BetaLets

        -- | Remove unused and non-effectful let bindings
        | DeadCode

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

        -- | Elaborate on implicit witnesses
        | Elaborate


-- | The result of a transform
data TransformResult r
	= TransformResult
	{ result   	 :: r
	, resultProgress :: Bool
	, resultInfo	 :: TransformInfo }


-- | Existential package for a TransformInfo.
data TransformInfo
	= forall i
        .  (Typeable i, Pretty i)
	=> TransformInfo i


-- | Create a result with no extra information
--   We say that progress is false to stop a fixpoint running.
resultSimple :: String -> r -> TransformResult r
resultSimple name r = TransformResult r False
		    $ TransformInfo $ NoInformation name


-- | Place-holder type to use when there is no real TransformResult.
data NoInformation 
        = NoInformation String
        deriving Typeable


instance Pretty NoInformation where
    ppr (NoInformation name) = text name P.<> text ": No information"



instance Pretty (Simplifier s a n) where
 ppr ss
  = case ss of
        Seq s1 s2
         -> ppr s1 <+> text "<>" <+> ppr s2

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
        BetaLets        -> text "BetaLets"
        DeadCode        -> text "DeadCode"
        Forward         -> text "Forward"
        Bubble          -> text "Bubble"
        Inline{}        -> text "Inline"
        Namify{}        -> text "Namify"
        Rewrite{}       -> text "Rewrite"
        Elaborate       -> text "Elaborate"


instance Pretty (TransformResult r) where
 ppr (TransformResult _ _ (TransformInfo i))
  = ppr i

