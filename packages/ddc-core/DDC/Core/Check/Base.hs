
module DDC.Core.Check.Base
        ( -- Things defined in this module.
          Config (..)
        , configOfProfile

        , CheckM
        , newExists
        , newPos
        , applyContext
        , applySolved

        , CheckTrace (..)
        , ctrace

          -- Things defined elsewhere.
        , throw, runCheck, evalCheck
        , EnvX,  EnvT, TypeEnv, KindEnv
        , Set
        , module DDC.Core.Check.Error
        , module DDC.Core.Collect
        , module DDC.Core.Pretty
        , module DDC.Core.Exp.Annot
        , module DDC.Type.Check.Context
        , module DDC.Type.DataDef
        , module DDC.Type.Universe
        , module DDC.Type.Exp.Simple
        , module DDC.Base.Pretty
        , module DDC.Data.ListUtils
        , module Control.Monad
        , module Data.Maybe)
where
import DDC.Core.Check.Error
import DDC.Core.Collect
import DDC.Core.Pretty
import DDC.Core.Exp.Annot
import DDC.Type.Check.Context
import DDC.Type.Check.Config
import DDC.Core.Env.EnvT                        (EnvT)
import DDC.Core.Env.EnvX                        (EnvX)
import DDC.Type.Env                             (TypeEnv, KindEnv)
import DDC.Type.DataDef
import DDC.Type.Universe
import DDC.Type.Exp.Simple
import DDC.Base.Pretty
import DDC.Control.Monad.Check                  (throw, runCheck, evalCheck)
import DDC.Data.ListUtils
import Control.Monad
import Data.Monoid                              hiding ((<>))
import Data.Maybe
import Data.Set                                 (Set)
import qualified Data.Set                       as Set
import qualified DDC.Type.Check.Error           as T
import qualified DDC.Type.Check.ErrorMessage    ()
import qualified DDC.Control.Monad.Check        as G
import Prelude                                  hiding ((<$>))


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
                -> throw $ ErrorType $ T.ErrorInfinite tExt tBind
        Right t -> return t


-- | Substitute solved constraints into a type.
applySolved :: Ord n => Context n -> Type n -> CheckM a n (Type n)
applySolved ctx tt
 = case applySolvedEither ctx Set.empty tt of
        Left  (tExt, tBind)
                -> throw $ ErrorType $ T.ErrorInfinite tExt tBind
        Right t -> return t



-- CheckTrace -----------------------------------------------------------------
-- | Human readable trace of the type checker.
data CheckTrace
        = CheckTrace
        { checkTraceDoc :: Doc }

instance Pretty CheckTrace where
 ppr ct = checkTraceDoc ct

instance Monoid CheckTrace where
 mempty = CheckTrace empty

 mappend ct1 ct2
        = CheckTrace
        { checkTraceDoc = checkTraceDoc ct1 <> checkTraceDoc ct2 }


-- | Append a doc to the checker trace.
ctrace :: Doc -> CheckM a n ()
ctrace doc'
 = do   (tr, ix, pos)       <- G.get
        let tr' = tr { checkTraceDoc = checkTraceDoc tr <$> doc' }
        G.put (tr', ix, pos)
        return  ()

