
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
        = Id
        | Anonymize
        | Snip
        | Flatten
        | Beta
        | Forward

        | Inline
                { transInlineDef   :: n -> Maybe (Exp a n) }

        | Namify
                { transMkNamifierT :: Env n -> Namifier s n
                , transMkNamifierX :: Env n -> Namifier s n }

        | Rewrite
                { transRules       :: [(String,RewriteRule a n)] }


-- | The result of a transform
data TransformResult a n
	= TransformResult
	{ resultExp	 :: Exp a n
	, resultProgress :: Bool
	, resultInfo	 :: TransformInfo }

data TransformInfo
	= forall i.
	(Typeable i, Pretty i)
	=> TransformInfo i


-- | Create a result with no extra information
-- We say that progress is false to stop a fixpoint running.
resultSimple :: Exp a n -> TransformResult a n
resultSimple x = TransformResult x False (TransformInfo NoInformation)

data NoInformation = NoInformation
    deriving Typeable
instance Pretty NoInformation where
    ppr NoInformation = text "No information available"

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
        Inline{}        -> text "Inline"
        Namify{}        -> text "Namify"
        Rewrite{}       -> text "Rewrite"


instance Pretty (TransformResult a n) where
 ppr (TransformResult _ _ (TransformInfo i))
  = ppr i

