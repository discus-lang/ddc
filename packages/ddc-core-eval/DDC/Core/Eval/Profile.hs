
-- | Core language profile for the evaluator.
module DDC.Core.Eval.Profile
        (evalProfile)
where
import DDC.Core.Fragment.Profile
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
        { featuresClosureTerms          = True
        , featuresPartialPrims          = False
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresLazyBindings          = True
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False
        , featuresNameShadowing         = True
        , featuresUnusedBindings        = True
        , featuresUnusedMatches         = True }

