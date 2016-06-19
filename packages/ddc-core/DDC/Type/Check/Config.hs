
module DDC.Type.Check.Config
        ( Config (..)
        , configTypeEqns
        , configOfProfile)
where
import DDC.Type.Exp
import DDC.Type.DataDef
import DDC.Type.Env                     (KindEnv, TypeEnv)
import Data.Map                         (Map)
import qualified DDC.Type.Env           as Env
import qualified DDC.Core.Fragment      as F
import qualified Data.Map               as Map


-- Config ---------------------------------------------------------------------
-- | Static configuration for the type checker.
--   These fields don't change as we decend into the tree.
--
--   The starting configuration should be converted from the profile that
--   defines the language fragment you are checking. 
--   See "DDC.Core.Fragment" and use `configOfProfile` below.
data Config n
        = Config
        { -- | Kinds of primitive types.
          configPrimKinds               :: KindEnv n

          -- | Types of primitive operators.
        , configPrimTypes               :: TypeEnv n

          -- | Data type definitions.
        , configDataDefs                :: DataDefs n  

          -- | Type equations, mapping the constructor name to its
          --   kind and associated type.
        , configTypeDefs                :: Map n (Kind n, Type n)

          -- | Types of globally available capabilities.
          --
          --   The inferred types of computations do not contain these
          --   capabilities as they are always available and thus do not
          --   need to be tracked in types.
        , configGlobalCaps              :: TypeEnv n

          -- | This name represents some hole in the expression that needs
          --   to be filled in by the type checker.
        , configNameIsHole              :: Maybe (n -> Bool) 

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

          -- | Allow general let-rec
        , configGeneralLetRec           :: Bool

          -- | Automatically run effectful applications.
        , configImplicitRun             :: Bool

          -- | Automatically box bodies of abstractions.
        , configImplicitBox             :: Bool
        }

configTypeEqns :: Config n -> Map n (Type n)
configTypeEqns config
        = Map.map snd $ configTypeDefs config


-- | Convert a language profile to a type checker configuration.
configOfProfile :: F.Profile n -> Config n
configOfProfile profile
 = let  features        = F.profileFeatures profile
   in   Config
        { configPrimKinds               = F.profilePrimKinds            profile
        , configPrimTypes               = F.profilePrimTypes            profile
        , configDataDefs                = F.profilePrimDataDefs         profile
        , configTypeDefs                = Map.empty
        , configGlobalCaps              = Env.empty
        , configNameIsHole              = F.profileNameIsHole           profile 
        
        , configTrackedEffects          = F.featuresTrackedEffects      features
        , configTrackedClosures         = F.featuresTrackedClosures     features
        , configFunctionalEffects       = F.featuresFunctionalEffects   features
        , configFunctionalClosures      = F.featuresFunctionalClosures  features
        , configEffectCapabilities      = F.featuresEffectCapabilities  features
        , configGeneralLetRec           = F.featuresGeneralLetRec       features
        , configImplicitRun             = F.featuresImplicitRun         features
        , configImplicitBox             = F.featuresImplicitBox         features

        }
        

