
module DDC.Type.Check.Config
        ( Config (..)
        , configOfProfile)
where
import DDC.Type.DataDef
import DDC.Type.Env                     (KindEnv, TypeEnv)
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
        { -- | Kinds of primitive types.
          configPrimKinds               :: KindEnv n

          -- | Types of primitive operators.
        , configPrimTypes               :: TypeEnv n

          -- | Data type definitions.
        , configDataDefs                :: DataDefs n  

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



-- | Convert a language profile to a type checker configuration.
configOfProfile :: F.Profile n -> Config n
configOfProfile profile
 = let  features        = F.profileFeatures profile
   in   Config
        { configPrimKinds               = F.profilePrimKinds  profile
        , configPrimTypes               = F.profilePrimTypes  profile
        , configDataDefs                = F.profilePrimDataDefs profile
        , configNameIsHole              = F.profileNameIsHole profile 
        
        , configTrackedEffects          = F.featuresTrackedEffects      features
        , configTrackedClosures         = F.featuresTrackedClosures     features
        , configFunctionalEffects       = F.featuresFunctionalEffects   features
        , configFunctionalClosures      = F.featuresFunctionalClosures  features
        , configEffectCapabilities      = F.featuresEffectCapabilities  features
        , configGeneralLetRec           = F.featuresGeneralLetRec       features
        , configImplicitRun             = F.featuresImplicitRun         features
        , configImplicitBox             = F.featuresImplicitBox         features

        }
        

