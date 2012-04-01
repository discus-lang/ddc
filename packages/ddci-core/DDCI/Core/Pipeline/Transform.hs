
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
        = None
        | Anonymize
        | ANormal
        | Beta
	| Rewrite
        deriving (Eq, Show)


parseTransform :: String -> Maybe Transform
parseTransform str
 = case str of
        "None"          -> Just None
        "Anonymize"     -> Just Anonymize
        "ANormal"	-> Just ANormal
        "Beta"          -> Just Beta
        "Rewrite"       -> Just Rewrite
        _               -> Nothing


instance Pretty Transform where
 ppr ss
  = case ss of
        None            -> text "None"
        Anonymize       -> text "Anonymize"
        ANormal         -> text "ANormal"
        Beta            -> text "Beta"
        Rewrite         -> text "Rewrite"


-- Apply ----------------------------------------------------------------------
applyTransform
        :: (Show a, Ord n)
        => Transform
        -> Module a n
        -> Module a n
applyTransform spec mm
 = case spec of
        None            -> mm
        Anonymize       -> anonymizeX mm
        _               -> error "applyTransform: finish me"


applyTransformX 
        :: (Show a, Ord Name)
        => Transform 
        -> [R.RewriteRule a Name] 
        -> Exp a Name 
        -> Exp a Name

applyTransformX spec rules xx
 = case spec of
        None            -> xx
        Anonymize       -> anonymizeX xx
        ANormal         -> anormalise xx
        Beta            -> betaReduce xx
        Rewrite         -> rewrite rules xx

