
-- | A language profile determines what features a program can use.
module DDC.Core.Fragment.Profile
        ( Profile (..)
        , zeroProfile

        , Features(..)
        , zeroFeatures
        , setFeature)
where
import DDC.Core.Fragment.Feature
import DDC.Type.DataDef
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env


-- | The profile holds the types of primitives, 
--   and a set of language features.
data Profile n
        = Profile
        { -- | The name of this profile.
          profileName                   :: String

          -- | Permitted language features.
        , profileFeatures               :: Features

          -- | Primitive operations
        , profilePrimDataDefs           :: DataDefs n
        , profilePrimKinds              :: Env n
        , profilePrimTypes              :: Env n  }


-- | A language profile with no features
zeroProfile :: Profile n
zeroProfile
        = Profile
        { profileName                   = "Zero"
        , profileFeatures               = zeroFeatures
        , profilePrimDataDefs           = emptyDataDefs
        , profilePrimKinds              = Env.empty
        , profilePrimTypes              = Env.empty }


-- | A flattened set of features, for easy lookup.
data Features 
        = Features
        { featuresPartialPrims          :: Bool
        , featuresGeneralApplication    :: Bool
        , featuresNestedFunctions       :: Bool
        , featuresLazyBindings          :: Bool
        , featuresDebruijnBinders       :: Bool
        , featuresUnboundLevel0Vars     :: Bool
        , featuresNameShadowing         :: Bool
        , featuresUnusedBindings        :: Bool
        , featuresUnusedMatches         :: Bool
        }


-- | An emtpy feature set, with all flags set to `False`.
zeroFeatures :: Features
zeroFeatures
        = Features
        { featuresPartialPrims          = False
        , featuresGeneralApplication    = False
        , featuresNestedFunctions       = False
        , featuresLazyBindings          = False
        , featuresDebruijnBinders       = False
        , featuresUnboundLevel0Vars     = False
        , featuresNameShadowing         = False
        , featuresUnusedBindings        = False
        , featuresUnusedMatches         = False }


-- | Set a language `Flag` in the `Profile`.
setFeature :: Feature -> Bool -> Features -> Features
setFeature feature val features
 = case feature of
        PartialPrims            -> features { featuresPartialPrims        = val }
        GeneralApplication      -> features { featuresGeneralApplication  = val }
        NestedFunctions         -> features { featuresNestedFunctions     = val }
        LazyBindings            -> features { featuresLazyBindings        = val }
        DebruijnBinders         -> features { featuresDebruijnBinders     = val }
        UnboundLevel0Vars       -> features { featuresUnboundLevel0Vars   = val }
        NameShadowing           -> features { featuresNameShadowing       = val }
        UnusedBindings          -> features { featuresUnusedBindings      = val }
        UnusedMatches           -> features { featuresUnusedMatches       = val }
