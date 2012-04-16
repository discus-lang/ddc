
-- | A language profile determines what features a program can use.
module DDC.Core.Language.Profile
        ( Profile (..)
        , zeroProfile

        , Features(..)
        , zeroFeatures
        , setFeature)
where
import DDC.Core.Language.Feature
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
        { -- General features
          featuresPartialApplication    :: Bool
        , featuresPartialPrims          :: Bool
        , featuresGeneralApplication    :: Bool
        , featuresNestedFunctions       :: Bool
        , featuresLazyBindings          :: Bool
        , featuresDataCtors             :: Bool
        , featuresDebruijnBinders       :: Bool

          -- Regions
        , featuresLetRegion             :: Bool
        , featuresMutableRegions        :: Bool
        , featuresLocalRegions          :: Bool
        , featuresGlobalRegions         :: Bool

          -- Foreign modules
        , featuresImports               :: Bool

          -- Sanity checking
        , featuresNameShadowing         :: Bool
        , featuresUnusedBindings        :: Bool
        , featuresUnusedMatches         :: Bool
        , featuresUnusedImports         :: Bool
        }


-- | An emtpy feature set, with all flags set to `False`.
zeroFeatures :: Features
zeroFeatures
        = Features
        { featuresPartialApplication    = False
        , featuresPartialPrims          = False
        , featuresGeneralApplication    = False
        , featuresNestedFunctions       = False
        , featuresLazyBindings          = False
        , featuresDataCtors             = False
        , featuresDebruijnBinders       = False
        , featuresLetRegion             = False
        , featuresMutableRegions        = False
        , featuresLocalRegions          = False
        , featuresGlobalRegions         = False
        , featuresImports               = False
        , featuresNameShadowing         = False
        , featuresUnusedBindings        = False
        , featuresUnusedMatches         = False
        , featuresUnusedImports         = False }


-- | Set a language `Flag` in the `Profile`.
setFeature :: Feature -> Bool -> Features -> Features
setFeature feature val features
 = case feature of
        PartialApplication      -> features { featuresPartialApplication  = val }
        PartialPrims            -> features { featuresPartialPrims        = val }
        GeneralApplication      -> features { featuresGeneralApplication  = val }
        NestedFunctions         -> features { featuresNestedFunctions     = val }
        LazyBindings            -> features { featuresLazyBindings        = val }
        DataCtors               -> features { featuresDataCtors           = val }
        DebruijnBinders         -> features { featuresDebruijnBinders     = val }
        LetRegion               -> features { featuresLetRegion           = val }
        MutableRegions          -> features { featuresMutableRegions      = val }
        LocalRegions            -> features { featuresLocalRegions        = val }
        GlobalRegions           -> features { featuresGlobalRegions       = val }
        Imports                 -> features { featuresImports             = val }
        NameShadowing           -> features { featuresNameShadowing       = val }
        UnusedBindings          -> features { featuresUnusedBindings      = val }
        UnusedMatches           -> features { featuresUnusedMatches       = val }
        UnusedImports           -> features { featuresUnusedImports       = val }
