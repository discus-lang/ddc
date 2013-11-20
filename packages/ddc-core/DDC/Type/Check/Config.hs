
module DDC.Type.Check.Config
        ( Config (..)
        , configOfProfile)
where
import DDC.Type.DataDef
import DDC.Type.Env                     (SuperEnv, KindEnv, TypeEnv)
import qualified DDC.Core.Fragment      as F


-- Config ---------------------------------------------------------------------
-- | Static configuration for the type checker.
--   These fields don't change as we decend into the tree.
--
--   The starting configuration should be converted from the profile that
--   defines the language fragment you are checking. 
--   See "DDC.Core.Fragment" and use `configOfProfile` below.
data Config n
        = Config
        { -- | Super kinds of primitive kinds.
          configPrimSupers              :: SuperEnv n

          -- | Kinds of primitive types.
        , configPrimKinds               :: KindEnv n

          -- | Types of primitive operators.
        , configPrimTypes               :: TypeEnv n

        -- | Data type definitions.
        , configDataDefs                :: DataDefs n  

          -- | Track effect type information.
        , configTrackedEffects          :: Bool

          -- | Track closure type information.
        , configTrackedClosures         :: Bool 

          -- | Attach effect information to function types.
        , configFunctionalEffects       :: Bool

          -- | Attach closure information to function types.
        , configFunctionalClosures      :: Bool

          -- | Treat effects as capabilities.
        , configEffectCapabilities      :: Bool 

          -- | This name represents some hole in the expression that needs
          --   to be filled in by the type checker.
        , configNameIsHole              :: Maybe (n -> Bool) }



-- | Convert a langage profile to a type checker configuration.
configOfProfile :: F.Profile n -> Config n
configOfProfile profile
        = Config
        { configPrimSupers         = F.profilePrimSupers profile
        , configPrimKinds          = F.profilePrimKinds  profile
        , configPrimTypes          = F.profilePrimTypes  profile

        , configDataDefs           = F.profilePrimDataDefs profile
        
        , configTrackedEffects     = F.featuresTrackedEffects
                                   $ F.profileFeatures profile

        , configTrackedClosures    = F.featuresTrackedClosures
                                   $ F.profileFeatures profile

        , configFunctionalEffects  = F.featuresFunctionalEffects
                                   $ F.profileFeatures profile

        , configFunctionalClosures = F.featuresFunctionalClosures
                                   $ F.profileFeatures profile 

        , configEffectCapabilities = F.featuresEffectCapabilities
                                   $ F.profileFeatures profile

        , configNameIsHole         = F.profileNameIsHole profile }
        

