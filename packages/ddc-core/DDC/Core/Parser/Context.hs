
module DDC.Core.Parser.Context 
        ( Context (..)
        , contextOfProfile)
where
import DDC.Core.Fragment


-- | Configuration and information from the context. 
--   Used for context sensitive parsing.
data Context
        = Context
        { contextTrackedEffects         :: Bool 
        , contextTrackedClosures        :: Bool
        , contextFunctionalEffects      :: Bool
        , contextFunctionalClosures     :: Bool }


-- | Slurp an initital Context from a Profile
contextOfProfile :: Profile n -> Context
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
        }
