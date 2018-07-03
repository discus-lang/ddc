
module DDC.Core.Check.Base
        ( -- Things defined in this module.
          Config (..)
        , configOfProfile

        , CheckM, G.MonadIO(..)
        , newExists
        , newPos
        , applyContext
        , applySolved

        , CheckTrace (..)
        , ctrace

          -- Things defined elsewhere.
        , throw, runCheck, evalCheck, mapErr
        , EnvX,  EnvT, TypeEnv, KindEnv
        , Set
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
import DDC.Control.CheckIO              (throw, runCheck, evalCheck, mapErr)
import DDC.Data.ListUtils

import Control.Monad
import Data.Maybe
import Data.Set                         (Set)
import DDC.Data.Pretty                  as P
import qualified Data.Set               as Set
import qualified DDC.Control.CheckIO    as G
import qualified Data.Semigroup         as SG
import Prelude                          hiding ((<$>))


-- | Type checker monad.
--   Used to manage type errors.
type CheckM a n
        = G.CheckM (CheckTrace, Int, Int) (Error a n)


-- | Allocate a new existential.
newExists :: Kind n -> CheckM a n (Exists n)
newExists k
 = do   (tr, ix, pos)       <- G.get
        G.put (tr, ix + 1, pos)
        return  (Exists ix k)


-- | Allocate a new context stack position.
newPos :: CheckM a n Pos
newPos
 = do   (tr, ix, pos)       <- G.get
        G.put (tr, ix, pos + 1)
        return  (Pos pos)


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


-- CheckTrace -----------------------------------------------------------------
-- | Human readable trace of the type checker.
data CheckTrace
        = CheckTrace
        { checkTraceDoc :: Doc }

instance Pretty CheckTrace where
 ppr ct         = checkTraceDoc ct

instance SG.Semigroup CheckTrace where
 (<>)           = unionCheckTrace

instance Monoid CheckTrace where
 mempty         = emptyCheckTrace
 mappend        = unionCheckTrace


-- | Construct an empty `CheckTrace`.
emptyCheckTrace :: CheckTrace
emptyCheckTrace = CheckTrace mempty


-- | Union two `ChecTrace`s.
unionCheckTrace :: CheckTrace -> CheckTrace -> CheckTrace
unionCheckTrace ct1 ct2
        = CheckTrace
        { checkTraceDoc = checkTraceDoc ct1 <> checkTraceDoc ct2 }


-- | Append a doc to the checker trace.
ctrace :: Doc -> CheckM a n ()
ctrace doc'
 = do   (tr, ix, pos)       <- G.get
        let tr' = tr { checkTraceDoc = checkTraceDoc tr `P.pline` doc' }
        G.put (tr', ix, pos)
        return  ()

