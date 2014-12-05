
module DDC.Core.Parser.Context 
        ( Context (..)
        , contextOfProfile)
where
import DDC.Core.Fragment
import DDC.Data.SourcePos
import Data.ByteString.Char8            (ByteString)


-- | Configuration and information from the context. 
--   Used for context sensitive parsing.
data Context n
        = Context
        { contextTrackedEffects         :: Bool 
        , contextTrackedClosures        :: Bool
        , contextFunctionalEffects      :: Bool
        , contextFunctionalClosures     :: Bool 

        , contextMakeStringName         :: SourcePos -> ByteString -> Maybe n }


-- | Slurp an initital `Context` from a language `Profile`.
contextOfProfile :: Profile n -> Context n
contextOfProfile profile
        = Context
        { contextTrackedEffects         = featuresTrackedEffects
                                        $ profileFeatures profile

        , contextTrackedClosures        = featuresTrackedClosures
                                        $ profileFeatures profile

        , contextFunctionalEffects      = featuresFunctionalEffects
                                        $ profileFeatures profile

        , contextFunctionalClosures     = featuresFunctionalClosures
                                        $ profileFeatures profile

        , contextMakeStringName         = \_ _ -> Nothing
        }
