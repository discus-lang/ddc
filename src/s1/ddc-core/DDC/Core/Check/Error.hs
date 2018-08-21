-- | Errors produced when checking core expressions.
module DDC.Core.Check.Error
        ( Error         (..)
        , ErrorType     (..)
        , ErrorData     (..)
        , checkOfResolveError)
where
import DDC.Core.Check.Error.ErrorExp
import DDC.Core.Check.Error.ErrorExpMessage   ()

import DDC.Core.Check.Error.ErrorType
import DDC.Core.Check.Error.ErrorTypeMessage  ()

import DDC.Core.Check.Error.ErrorData
import DDC.Core.Check.Error.ErrorDataMessage  ()

import DDC.Core.Exp
import qualified DDC.Core.Interface.Store       as Store

---------------------------------------------------------------------------------------------------
-- TODO: convert the rest of the errors.
-- TODO: add tests for these errors.
checkOfResolveError :: (Ord n, Show n) => n -> Store.Error n -> Error a n
checkOfResolveError n err
 = case err of
        Store.ErrorNotFound _   -> ErrorType $ ErrorTypeUndefinedTypeCtor (UName n)
        _                       -> error $ "some error " ++ show err

