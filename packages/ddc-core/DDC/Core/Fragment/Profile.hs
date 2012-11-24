
-- | A fragment profile determines what features a program can use.
module DDC.Core.Fragment.Profile
        ( Profile (..)
        , zeroProfile

        , Features(..)
        , zeroFeatures
        , setFeature)
where
import DDC.Core.Fragment.Feature
import DDC.Type.DataDef
import DDC.Type.Exp
import DDC.Type.Env                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env           as Env


-- | The fragment profile describes the language features and 
--   primitive operators available in the language.
data Profile n
        = Profile
        { -- | The name of this profile.
          profileName                   :: String

          -- | Permitted language features.
        , profileFeatures               :: Features

          -- | Primitive data type declarations.
        , profilePrimDataDefs           :: DataDefs n

          -- | Kinds of primitive types.
        , profilePrimKinds              :: KindEnv n

          -- | Types of primitive operators.
        , profilePrimTypes              :: TypeEnv n

          -- | Check whether a type is an unboxed type.
          --   Some fragments limit how these can be used.
        , profileTypeIsUnboxed          :: Type n -> Bool }


-- | A language profile with no features or primitive operators.
--
--   This provides a simple first-order language.
zeroProfile :: Profile n
zeroProfile
        = Profile
        { profileName                   = "Zero"
        , profileFeatures               = zeroFeatures
        , profilePrimDataDefs           = emptyDataDefs
        , profilePrimKinds              = Env.empty
        , profilePrimTypes              = Env.empty
        , profileTypeIsUnboxed          = const False }


-- | A flattened set of features, for easy lookup.
data Features 
        = Features
        { featuresClosureTerms          :: Bool
        , featuresPartialPrims          :: Bool
        , featuresPartialApplication    :: Bool
        , featuresGeneralApplication    :: Bool
        , featuresNestedFunctions       :: Bool
        , featuresLazyBindings          :: Bool
        , featuresDebruijnBinders       :: Bool
        , featuresUnboundLevel0Vars     :: Bool
        , featuresUnboxedInstantiation  :: Bool
        , featuresNameShadowing         :: Bool
        , featuresUnusedBindings        :: Bool
        , featuresUnusedMatches         :: Bool
        }


-- | An emtpy feature set, with all flags set to `False`.
zeroFeatures :: Features
zeroFeatures
        = Features
        { featuresClosureTerms          = False
        , featuresPartialPrims          = False
        , featuresPartialApplication    = False
        , featuresGeneralApplication    = False
        , featuresNestedFunctions       = False
        , featuresLazyBindings          = False
        , featuresDebruijnBinders       = False
        , featuresUnboundLevel0Vars     = False
        , featuresUnboxedInstantiation  = False
        , featuresNameShadowing         = False
        , featuresUnusedBindings        = False
        , featuresUnusedMatches         = False }


-- | Set a language `Flag` in the `Profile`.
setFeature :: Feature -> Bool -> Features -> Features
setFeature feature val features
 = case feature of
        ClosureTerms            -> features { featuresClosureTerms         = val }
        PartialPrims            -> features { featuresPartialPrims         = val }
        PartialApplication      -> features { featuresPartialApplication   = val }
        GeneralApplication      -> features { featuresGeneralApplication   = val }
        NestedFunctions         -> features { featuresNestedFunctions      = val }
        LazyBindings            -> features { featuresLazyBindings         = val }
        DebruijnBinders         -> features { featuresDebruijnBinders      = val }
        UnboundLevel0Vars       -> features { featuresUnboundLevel0Vars    = val }
        UnboxedInstantiation    -> features { featuresUnboxedInstantiation = val }
        NameShadowing           -> features { featuresNameShadowing        = val }
        UnusedBindings          -> features { featuresUnusedBindings       = val }
        UnusedMatches           -> features { featuresUnusedMatches        = val }

