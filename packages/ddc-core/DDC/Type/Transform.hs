
module DDC.Type.Transform
        ( -- * Anonymize named binders
          Anonymize     (..)
          
          -- * Rename named binders
        , Rename        (..)

          -- * Spread types into bound variables
        , Spread        (..)

          -- * Lift type indices
        , LiftT         (..)

          -- * Type substitution
        , SubstituteT   (..))
where
import DDC.Type.Transform.Anonymize
import DDC.Type.Transform.Rename
import DDC.Type.Transform.Spread
import DDC.Type.Transform.LiftT
import DDC.Type.Transform.SubstituteT
