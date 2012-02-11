
module DDCI.Core.Transform
        ( Transform (..)
        , parseTransform
        , applyTransformX)
where
import DDC.Base.Pretty
import DDC.Core.Exp
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.ANormal
import DDC.Core.Transform.Beta
import DDCI.Core.Eval.Name


-- | Desription of the transforms to apply to a core program.
data Transform
        = None
        | Anonymize
        | ANormal
        | Beta
        deriving (Eq, Show)

parseTransform :: String -> Maybe Transform
parseTransform str
 = case str of
        "None"          -> Just None
        "Anonymize"     -> Just Anonymize
        "ANormal"	-> Just ANormal
        "Beta"          -> Just Beta
        _               -> Nothing

instance Pretty Transform where
 ppr ss
  = case ss of
        None            -> text "None"
        Anonymize       -> text "Anonymize"
        ANormal         -> text "ANormal"
        Beta            -> text "Beta"


-- Apply ----------------------------------------------------------------------
applyTransformX 
        :: Ord Name 
        => Transform -> Exp a Name -> Exp a Name

applyTransformX spec xx
 = case spec of
        None            -> xx
        Anonymize       -> anonymizeX xx
        ANormal         -> anormalise xx
        Beta            -> betaReduce xx

