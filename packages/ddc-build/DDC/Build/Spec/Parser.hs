
module DDC.Build.Spec.Parser
        ( Error(..)
        , parseBuildSpec)
where
import DDC.Build.Spec.Base

data Error 
        = ErrorParse
        { errorParse    :: String}
        deriving Show

parseBuildSpec :: String -> Either Error Spec
parseBuildSpec _str
        = Right 
        $ Spec "foo" []
