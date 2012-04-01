
module DDCI.Core.Pipeline.Transform
        ( Transform (..)
        , parseTransform
        , applyTransform
        , applyTransformX)
where
import DDC.Base.Pretty
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.ANormal
import DDC.Core.Transform.Beta
import DDC.Core.Transform.Rewrite
import qualified DDC.Core.Transform.Rewrite.Rule as R
import DDC.Core.Eval.Name


-- | Desription of the transforms to apply to a core program.
data Transform
        = TransformId
        | TransformAnonymize
        | TransformANormal
        | TransformBeta
	| TransformRewrite
        deriving (Eq, Show)


parseTransform :: String -> Maybe Transform
parseTransform str
 = case str of
        "None"                  -> Just TransformId
        "Anonymize"             -> Just TransformAnonymize
        "ANormal"               -> Just TransformANormal
        "Beta"                  -> Just TransformBeta
        "Rewrite"               -> Just TransformRewrite
        _                       -> Nothing


instance Pretty Transform where
 ppr ss
  = case ss of
        TransformId             -> text "None"
        TransformAnonymize      -> text "Anonymize"
        TransformANormal        -> text "ANormal"
        TransformBeta           -> text "Beta"
        TransformRewrite        -> text "Rewrite"


-- Apply ----------------------------------------------------------------------
applyTransform
        :: (Show a, Ord n)
        => Transform
        -> Module a n
        -> Module a n
applyTransform spec mm
 = case spec of
        TransformId             -> mm
        TransformAnonymize      -> anonymizeX mm
--        TransformANormal        -> anormalise
        _                       -> error "applyTransform: finish me"


applyTransformX 
        :: (Show a, Ord Name)
        => Transform 
        -> [R.RewriteRule a Name] 
        -> Exp a Name 
        -> Exp a Name

applyTransformX spec rules xx
 = case spec of
        TransformId             -> xx
        TransformAnonymize      -> anonymizeX xx
        TransformANormal        -> anormalise xx
        TransformBeta           -> betaReduce xx
        TransformRewrite        -> rewrite rules xx

