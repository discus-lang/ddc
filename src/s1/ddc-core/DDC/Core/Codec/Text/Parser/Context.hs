{-# OPTIONS_HADDOCK hide #-}

module DDC.Core.Codec.Text.Parser.Context
        ( Context (..)
        , contextOfProfile)
where
import DDC.Core.Exp.Literal
import DDC.Core.Fragment
import DDC.Data.SourcePos


-- | Configuration and information from the context.
--   Used for context sensitive parsing.
data Context n
        = Context
        { contextTrackedEffects         :: Bool
        , contextTrackedClosures        :: Bool
        , contextFunctionalEffects      :: Bool
        , contextFunctionalClosures     :: Bool

          -- | Check whether the given fragment includes literals of this sort,
          --   and convert it to the appropriate primitive name.
        , contextMakeLiteralName
                :: Maybe (SourcePos -> Literal -> Bool -> Maybe n) }


-- | Slurp an initital `Context` from a language `Profile`.
contextOfProfile :: Profile n -> Context n
contextOfProfile profile
        = Context
        { contextTrackedEffects
                = featuresTrackedEffects
                $ profileFeatures profile

        , contextTrackedClosures
                = featuresTrackedClosures
                $ profileFeatures profile

        , contextFunctionalEffects
                = featuresFunctionalEffects
                $ profileFeatures profile

        , contextFunctionalClosures
                = featuresFunctionalClosures
                $ profileFeatures profile

        , contextMakeLiteralName
                = profileMakeLiteralName profile
        }
