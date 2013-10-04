module DDC.Core.Flow.Transform.Rates.Fail
        ( Fail (..)
        , LogFailures
        , warn, run)
where

import DDC.Core.Flow.Prim
import Control.Monad.Writer
import Data.List

-- | Why can't rates be inferred?
data Fail
        -- | Function is not in a-normal form
        = FailNotANormalForm
        -- | Bindings must be unique
        | FailNamesNotUnique
        -- | Bindings must be named
        | FailNoDeBruijnAllowed
        -- | Function contains letrec
        | FailRecursiveBindings
        -- | Function contains letregion
        | FailLetRegionNotHandled
        -- | The constraint would require a buffer. User must expicitly buffer.
        | FailConstraintFilteredLessFiltered Name Name
        -- | The constraint would require a buffer. User must expicitly buffer.
        | FailConstraintFilteredNotUnique    Name Name
        deriving (Show, Eq)


type LogFailures a = Writer [Fail] a

warn    :: Fail -> LogFailures ()
warn  w = tell [w]

run     :: LogFailures a -> (a, [Fail])
run comp
 = case runWriter comp of
   (a, warns) -> (a, nub warns)

