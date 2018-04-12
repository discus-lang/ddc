{-# LANGUAGE CPP #-}

-- You can't be serious..

#if __GLASGOW_HASKELL__ >= 741
module DDC.Data.Monoidal
        ( Monoid        (..)
        , Semigroup     (..)
        , module Control.Monad)
where
import Control.Monad
import Data.Monoid    (Monoid(..))
import Data.Semigroup (Semigroup(..))

#else
module DDC.Data.Monoidal
        ( Monoid        (..)
        , Semigroup     (..)
        , module Control.Monad
        , (<>))
where
import Control.Monad
import Data.Semigroup (Monoid(..), Semigroup(..))
#endif

