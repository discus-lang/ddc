
module DDC.Core.Check.Context
        ( 
          -- * Type Checker Mode.
          Mode    (..)

          -- * Positions in the Context.
        , Pos     (..)

          -- * Roles of Type Variable.
        , Role    (..)

          -- * Existentials
        , Exists  (..)
        , typeOfExists
        , takeExists
        , slurpExists

          -- * Context Elements.
        , Elem    (..)

          -- * Checker Context.
        , Context (..)

          -- * Construction
        , emptyContext
        , contextOfEnvT
        , contextOfEnvX
        , contextOfPrimEnvs

          -- * Projection
        , contextEquations
        , contextCapabilities
        , contextDataDefs
        , contextEnvT

          -- * Pushing
        , pushType,   pushTypes
        , pushKind,   pushKinds
        , pushExists, pushExistsBefore, pushExistsScope

          -- * Marking
        , markContext

          -- * Popping
        , popToPos

          -- * Lookup
        , lookupType
        , lookupKind
        , lookupExistsEq

          -- * Membership
        , memberType
        , memberKind
        , memberKindBind

          -- * Existentials
        , locationOfExists
        , updateExists

          -- * Lifting and Lowering
        , liftTypes
        , lowerTypes

          -- * Applying
        , applyContextEither
        , applySolvedEither

          -- * Effects
        , effectSupported

          -- * Implicits
        , findImplicitOfType)
where
import DDC.Core.Check.Context.Implicit
import DDC.Core.Check.Context.Effect
import DDC.Core.Check.Context.Apply
import DDC.Core.Check.Context.Elem
import DDC.Core.Check.Context.Mode
import DDC.Core.Check.Context.Base

