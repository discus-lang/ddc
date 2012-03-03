
module DDC.Core.Sea.Output.Profile
        (outputProfile)
where
import DDC.Core.DataDef
import DDC.Core.Language.Profile
import DDC.Core.Sea.Output.Name
import qualified DDC.Type.Env           as Env


-- | Profile of the core language fragment that can be converted
--   directly to the Sea language.
outputProfile :: Profile Name 
outputProfile
        = Profile
        { profileName           = "SeaOutput"
        , profileFeatures       = outputFeatures
        , profilePrimDataDefs   = emptyDataDefs
        , profilePrimKinds      = Env.empty
        , profilePrimTypes      = Env.empty }


outputFeatures :: Features
outputFeatures = zeroFeatures

