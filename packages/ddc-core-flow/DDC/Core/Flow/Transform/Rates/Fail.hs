module DDC.Core.Flow.Transform.Rates.Fail
        ( Fail (..)
        , LogFailures
        , warn, run)
where
import DDC.Core.Flow.Prim
import DDC.Base.Pretty
import Control.Monad.Writer
import Data.List

-- | Why couldn't it be converted to CNF?
data ConversionError
        -- | Function is not in a-normal form
        = FailNotANormalForm

        -- | Bindings must be unique
        | FailNamesNotUnique

        -- | Bindings must be named
        | FailNoDeBruijnAllowed

        -- | Bindings cannot be anonymous _.
        | FailNoAnonAllowed

        -- | Function contains letrec
        | FailRecursiveBindings

        -- | Function contains letregion
        | FailLetRegionNotHandled


-- | Why can't rates be inferred?
data Fail
        -- | The function couldn't be converted to combinator form
        = FailCannotConvert ConversionError
        -- | The constraint would require a buffer. User must expicitly buffer.
        | FailConstraintFilteredLessFiltered Name Name

        -- | The constraint would require a buffer. User must expicitly buffer.
        | FailConstraintFilteredNotUnique    Name Name
        deriving (Show, Eq)


instance Pretty Fail where
 ppr fails = text (show fails)


type LogFailures a = Writer [Fail] a

warn    :: Fail -> LogFailures ()
warn  w = tell [w]

run     :: LogFailures a -> (a, [Fail])
run comp
 = case runWriter comp of
   (a, warns) -> (a, nub warns)

