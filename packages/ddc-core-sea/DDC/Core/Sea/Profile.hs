
module DDC.Core.Sea.Profile
        (outputProfile)
where
import DDC.Core.Language.Profile
import DDC.Core.Sea.Name
import qualified DDC.Type.Env           as Env

-- | Profile of the core language fragment that can be converted
--   directly to the Sea language.
outputProfile :: Profile Name 
outputProfile
        = Profile
        { profileName           = "SeaOutput"
        , profileFeatures       = outputFeatures
        , profilePrimKinds      = Env.empty
        , profilePrimTypes      = Env.empty }

outputFeatures :: Features
outputFeatures = zeroFeatures

