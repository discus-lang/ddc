
-- | Collecting sets of variables and constructors.
module DDC.Core.Collect
        ( -- * Free Variables
          freeT
        , freeX

          -- * Bounds and Binds
        , collectBound
        , collectBinds

          -- * Support
        , Support       (..)
        , SupportX      (..))
where
import DDC.Core.Collect.Free
import DDC.Core.Collect.Free.Simple     ()
import DDC.Core.Collect.Support
import DDC.Type.Collect
