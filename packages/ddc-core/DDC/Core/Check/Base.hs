
module DDC.Core.Check.Base
        ( -- Things defined in this module.
          Config (..)
        , configOfProfile

        , CheckM (..)
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


-- | Type checker monad. 
--   Used to manage type errors.
type CheckM a n   
        = G.CheckM (CheckTrace, Int, Int) (Error a n)

-- | Allocate a new exisistential.
newExists :: Kind n -> CheckM a n (Exists n)
newExists k
 = do   (tr, ix, pos)       <- G.get
        G.put (tr, ix + 1, pos)
        return  (Exists ix k)


-- | Allocate a new stack position.
newPos :: CheckM a n Pos
newPos
 = do   (tr, ix, pos)       <- G.get
        G.put (tr, ix, pos + 1)
        return  (Pos pos)


-- CheckTrace -----------------------------------------------------------------
-- Trace for the type checker.
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


-- Type -----------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM 
        :: (Ord n, Show n, Pretty n) 
        => Config n 
        -> KindEnv n 
        -> Context n 
        -> Type n
        -> CheckM a n (Type n, Kind n)

checkTypeM config kenv ctx tt
 = case T.checkTypeWithContext config kenv ctx tt of
        Left err        -> throw $ ErrorType err
        Right (t, k)    -> return (t, k)


-- Bind -----------------------------------------------------------------------
-- | Check a bind.
checkBindM
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> KindEnv n
        -> Context n
        -> Bind n
        -> CheckM a n (Bind n, Kind n)

checkBindM config kenv ctx bb
 = case T.checkTypeWithContext config kenv ctx (typeOfBind bb) of
        Left err        -> throw $ ErrorType err
        Right (t', k)   -> return (replaceTypeOfBind t' bb, k)
