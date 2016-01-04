
module DDC.Core.Check.Base
        ( -- Things defined in this module.
          Config (..)
        , configOfProfile

        , CheckM
        , newExists
        , newPos

        , CheckTrace (..)
        , ctrace

        , checkTypeM
        , checkBindM

          -- Things defined elsewhere.
        , throw, runCheck, evalCheck
        , KindEnv, TypeEnv
        , Set
        , module DDC.Core.Check.Error
        , module DDC.Core.Collect
        , module DDC.Core.Predicates
        , module DDC.Core.Compounds
        , module DDC.Core.Pretty
        , module DDC.Core.Exp
        , module DDC.Type.Check.Context
        , module DDC.Type.DataDef
        , module DDC.Type.Equiv
        , module DDC.Type.Universe
        , module DDC.Type.Compounds
        , module DDC.Type.Predicates
        , module DDC.Type.Exp
        , module DDC.Base.Pretty
        , module DDC.Data.ListUtils
        , module Control.Monad
        , module Data.Maybe)
where
import DDC.Core.Check.Error
import DDC.Core.Collect
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Pretty
import DDC.Core.Exp
import DDC.Type.Check.Context
import DDC.Type.Check                           (Config (..), configOfProfile)
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Type.DataDef
import DDC.Type.Equiv
import DDC.Type.Universe
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Exp
import DDC.Base.Pretty
import DDC.Control.Monad.Check                  (throw, runCheck, evalCheck)
import DDC.Data.ListUtils
import Control.Monad
import Data.Monoid                              hiding ((<>))
import Data.Maybe
import Data.Set                                 (Set)
import qualified DDC.Type.Check                 as T
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


-- Bind -----------------------------------------------------------------------
-- | Check the type of a bind.
checkBindM
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Checker configuration.
        -> KindEnv n            -- ^ Global kind environment.
        -> Context n            -- ^ Local context.
        -> Universe             -- ^ Universe for the type of the bind.
        -> Bind n               -- ^ Check this bind.
        -> Mode n               -- ^ Mode for bidirectional checking.
        -> CheckM a n (Bind n, Type n, Context n)

checkBindM config kenv ctx uni bb mode
 = do   (t', k, ctx')   <- checkTypeM config kenv ctx uni
                                (typeOfBind bb) mode
        return (replaceTypeOfBind t' bb, k, ctx')


-- Type -----------------------------------------------------------------------
-- | Check a type in the exp checking monad, returning its kind.
checkTypeM
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Checker configuration.
        -> KindEnv n            -- ^ Global kind environment.
        -> Context n            -- ^ Local context.
        -> Universe             -- ^ Universe the type is supposed to be in.
        -> Type n               -- ^ Check this type.
        -> Mode n               -- ^ Mode for bidirectional checking
        -> CheckM a n (Type n, Kind n, Context n)

checkTypeM config kenv ctx uni tt mode
 = do
        -- Run the inner type/kind checker computation,
        -- giving it our current values for the existential and position
        -- generators.
        (tr, ix, pos)   <- G.get

        let ((ix', pos'), result)
                = G.runCheck (ix, pos)
                $ T.checkTypeM config kenv ctx uni tt mode

        G.put (tr, ix', pos')

        -- If the type/kind checker returns an error then wrap it
        -- so we can throw it from our exp/type checker.
        case result of
         Left err
          -> throw $ ErrorType err

         Right (t, k, ctx')
          -> return (t, k, ctx')

