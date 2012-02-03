
module DDCI.Core.Transform
        ( Transform (..)
        , parseTransform
        , applyTransformX)
where
import DDC.Base.Pretty
import DDC.Core.Exp
import DDC.Core.Transform.Anonymize


-- | Desription of the transforms to apply to a core program.
data Transform
        = None
        | Anonymize
        deriving (Eq, Show)

parseTransform :: String -> Maybe Transform
parseTransform str
 = case str of
        "None"          -> Just None
        "Anonymize"     -> Just Anonymize
        _               -> Nothing

instance Pretty Transform where
 ppr ss
  = case ss of
        None            -> text "None"
        Anonymize       -> text "Anonymize"


-- Apply ----------------------------------------------------------------------
applyTransformX 
        :: Ord Name 
        => Transform -> Exp a Name -> Exp a Name

applyTransformX spec xx
 = case spec of
        None            -> xx
        Anonymize       -> anonymize [] xx
