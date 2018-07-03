
module DDC.Core.Check.Base
        ( -- Things defined in this module.
          Config (..)
        , configOfProfile

        , applyContext
        , applySolved

          -- Things defined elsewhere.
        , EnvX,  EnvT, TypeEnv, KindEnv
        , Set
        , module DDC.Core.Check.State
        , module DDC.Core.Check.Error
        , module DDC.Core.Collect
        , module DDC.Core.Codec.Text.Pretty
        , module DDC.Core.Exp.Annot
        , module DDC.Core.Check.Context

        , module DDC.Type.DataDef
        , module DDC.Type.Universe
        , module DDC.Type.Exp.Simple
        , module DDC.Data.ListUtils
        , module Control.Monad
        , module Data.Maybe)
where
import DDC.Core.Check.State
import DDC.Core.Check.Error
import DDC.Core.Collect
import DDC.Core.Codec.Text.Pretty
import DDC.Core.Exp.Annot
import DDC.Core.Check.Context
import DDC.Core.Check.Config
import DDC.Core.Env.EnvT                (EnvT)
import DDC.Core.Env.EnvX                (EnvX)

import DDC.Type.Env                     (TypeEnv, KindEnv)
import DDC.Type.DataDef
import DDC.Type.Universe
import DDC.Type.Exp.Simple
import DDC.Data.ListUtils

import Control.Monad
import Data.Maybe
import Data.Set                         (Set)
import qualified Data.Set               as Set
import Prelude                          hiding ((<$>))


-- | Apply the checker context to a type.
applyContext :: Ord n => Context n -> Type n -> CheckM a n (Type n)
applyContext ctx tt
 = case applyContextEither ctx Set.empty tt of
        Left  (tExt, tBind)
                -> throw $ ErrorType $ ErrorTypeInfinite tExt tBind
        Right t -> return t


-- | Substitute solved constraints into a type.
applySolved :: Ord n => Context n -> Type n -> CheckM a n (Type n)
applySolved ctx tt
 = case applySolvedEither ctx Set.empty tt of
        Left  (tExt, tBind)
                -> throw $ ErrorType $ ErrorTypeInfinite tExt tBind
        Right t -> return t


