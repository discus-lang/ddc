
-- | Collecting sets of variables and constructors.
module DDC.Core.Collect
        ( -- * Free Variables
          freeT,        freeVarsT
        , freeX

          -- * Bounds and Binds
        , collectBound
        , collectBinds

          -- * Abstract Binding Structures
        , BindTree      (..)
        , BindWay       (..)
        , BoundLevel    (..)
        , BindStruct    (..)

          -- * Support
        , Support       (..)
        , SupportX      (..))
where
import DDC.Core.Collect.FreeX
import DDC.Core.Collect.FreeT
import DDC.Core.Collect.BindStruct
import DDC.Core.Collect.Support
