
-- | The ambient Disciple Core language is specialised to concrete languages
--   by adding primitive operations and optionally restricting the set of 
--   available language features. This specialisation results in user-facing
--   language fragments such as @Disciple Core Lite@ and @Disciple Core Salt@.
module DDC.Core.Fragment
        ( -- * Fragment Profiles
          Profile       (..)
        , zeroProfile

          -- * Fragment Features
        , Feature       (..)
        , Features      (..)
        , zeroFeatures

        -- * Compliance
        , complies
        , compliesWithEnvs
        , Complies
        , Error         (..))
where
import DDC.Core.Fragment.Feature
import DDC.Core.Fragment.Compliance
import DDC.Core.Fragment.Error
import DDC.Core.Fragment.Profile
