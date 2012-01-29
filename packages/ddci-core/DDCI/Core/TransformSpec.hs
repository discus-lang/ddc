
module DDCI.Core.TransformSpec
        ( TransformSpec (..)
        , parseTransformSpec)
where
import DDC.Base.Pretty


-- | Desription of the transforms to apply to a core program.
data TransformSpec
        = None
        | Anonymize
        deriving (Eq, Show)

parseTransformSpec :: String -> Maybe TransformSpec
parseTransformSpec str
 = case str of
        "None"          -> Just None
        "Anonymize"     -> Just Anonymize
        _               -> Nothing

instance Pretty TransformSpec where
 ppr ss
  = case ss of
        None            -> text "None"
        Anonymize       -> text "Anonymize"
