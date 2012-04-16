
-- | Core language profile for the evaluator.
module DDC.Core.Eval.Profile
        (evalProfile)
where
import DDC.Core.Language.Profile
import DDC.Core.Eval.Env
import DDC.Core.Eval.Name


-- | Core language fragment that can be directly evaluated.
evalProfile :: Profile Name 
evalProfile
        = Profile
        { profileName                   = "Eval"
        , profileFeatures               = evalFeatures
        , profilePrimDataDefs           = primDataDefs
        , profilePrimKinds              = primKindEnv
        , profilePrimTypes              = primTypeEnv }


evalFeatures :: Features
evalFeatures 
        = Features
        { featuresPartialApplication    = True
        , featuresPartialPrims          = False
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresLazyBindings          = True
        , featuresDataCtors             = True
        , featuresDebruijnBinders       = True
        , featuresLetRegion             = True
        , featuresMutableRegions        = True
        , featuresLocalRegions          = True
        , featuresGlobalRegions         = True
        , featuresImports               = True
        , featuresNameShadowing         = True
        , featuresUnusedBindings        = True
        , featuresUnusedMatches         = True
        , featuresUnusedImports         = True }

