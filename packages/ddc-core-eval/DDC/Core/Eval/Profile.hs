
-- | Core language profile for the evaluator.
module DDC.Core.Eval.Profile
        (evalProfile)
where
import DDC.Core.Fragment
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
        , profilePrimTypes              = primTypeEnv 
        , profileTypeIsUnboxed          = const False }


-- | Language features used by the eval fragment.
evalFeatures :: Features
evalFeatures 
        = Features
        { featuresClosureTerms          = True
        , featuresPartialPrims          = False
        , featuresPartialApplication    = True
        , featuresGeneralApplication    = True
        , featuresNestedFunctions       = True
        , featuresLazyBindings          = True
        , featuresDebruijnBinders       = True
        , featuresUnboundLevel0Vars     = False
        , featuresUnboxedInstantiation  = True
        , featuresNameShadowing         = True
        , featuresUnusedBindings        = True
        , featuresUnusedMatches         = True }

