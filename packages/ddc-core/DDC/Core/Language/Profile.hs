
-- | A language profile determines what features a program can use.
module DDC.Core.Language.Profile
        ( Profile (..)
        , Features(..)
        , setFeature)
where
import DDC.Core.Language.Feature
import DDC.Type.Env     (Env)


-- | The profile holds the types of primitives, and a flattened set of language
--   flags. See `Flag` for a description of these.
data Profile n
        = Profile
        { -- | The name of this profile.
          profileName                   :: String

          -- | Permitted language features.
        , profileFeatures               :: Features

          -- | Primitive operations
        , profilePrimKinds              :: Env n
        , profilePrimTypes              :: Env n  }


-- | A flattened set of features, for easy lookup.
data Features 
        = Features
        { -- Primitive operations
          featuresPartialPrims           :: Bool

          -- General features
        , featuresPartialApplication     :: Bool
        , featuresNestedFunctions        :: Bool
        , featuresLazyBindings           :: Bool
        , featuresDataCtors              :: Bool

          -- Regions
        , featuresLetRegion              :: Bool
        , featuresMutableRegions         :: Bool
        , featuresLocalRegions           :: Bool
        , featuresGlobalRegions          :: Bool

          -- Foreign modules
        , featuresImports                :: Bool

          -- Sanity checking
        , featuresNameShadowing          :: Bool
        , featuresUnusedBindings         :: Bool
        , featuresUnusedMatches          :: Bool
        , featuresUnusedImports          :: Bool
        }


-- | Set a language `Flag` in the `Profile`.
setFeature :: Feature -> Bool -> Features -> Features
setFeature feature val features
 = case feature of
        PartialPrims            -> features { featuresPartialPrims        = val }
        PartialApplication      -> features { featuresPartialApplication  = val }
        NestedFunctions         -> features { featuresNestedFunctions     = val }
        LazyBindings            -> features { featuresLazyBindings        = val }
        DataCtors               -> features { featuresDataCtors           = val }
        LetRegion               -> features { featuresLetRegion           = val }
        MutableRegions          -> features { featuresMutableRegions      = val }
        LocalRegions            -> features { featuresLocalRegions        = val }
        GlobalRegions           -> features { featuresGlobalRegions       = val }
        Imports                 -> features { featuresImports             = val }
        NameShadowing           -> features { featuresNameShadowing       = val }
        UnusedBindings          -> features { featuresUnusedBindings      = val }
        UnusedMatches           -> features { featuresUnusedMatches       = val }
        UnusedImports           -> features { featuresUnusedImports       = val }
