-- | Errors produced when checking core expressions.
module DDC.Core.Check.Error
        ( Error         (..)
        , ErrorType     (..)
        , ErrorData     (..))
where
import DDC.Core.Check.Error.ErrorExp
import DDC.Core.Check.Error.ErrorExpMessage   ()

import DDC.Core.Check.Error.ErrorType
import DDC.Core.Check.Error.ErrorTypeMessage  ()

import DDC.Core.Check.Error.ErrorData
import DDC.Core.Check.Error.ErrorDataMessage  ()



