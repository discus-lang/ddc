
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
        { -- | Data type definitions.
          configPrimDataDefs            :: DataDefs n 

          -- | Super kinds of primitive kinds.
        , configPrimSupers              :: SuperEnv n

          -- | Kinds of primitive types.
        , configPrimKinds               :: KindEnv n

          -- | Types of primitive operators.
        , configPrimTypes               :: TypeEnv n

          -- | Suppress effect information,
          --   annotating all functions with a pure effect.
        , configSuppressEffects         :: Bool

          -- | Suppress closure information, 
          --   annotating all functions with an empty closure.
        , configSuppressClosures        :: Bool }


-- | Convert a langage profile to a type checker configuration.
configOfProfile :: F.Profile n -> Config n
configOfProfile profile
        = Config
        { configPrimDataDefs    = F.profilePrimDataDefs profile
        , configPrimSupers      = F.profilePrimSupers profile
        , configPrimKinds       = F.profilePrimKinds  profile
        , configPrimTypes       = F.profilePrimTypes  profile

        , configSuppressEffects
                = F.featuresUntrackedEffects
                $ F.profileFeatures profile

        , configSuppressClosures      
                = F.featuresUntrackedClosures
                $ F.profileFeatures profile }

