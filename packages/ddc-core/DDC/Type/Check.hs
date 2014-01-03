-- | Check the kind of a type.
module DDC.Type.Check
        ( Config        (..)
        , configOfProfile

          -- * Kinds of Types
        , checkType
        , checkTypeWithContext
        , kindOfType

          -- * Kinds of Constructors
        , takeSortOfKiCon
        , kindOfTwCon
        , kindOfTcCon
        
          -- * Errors
        , Error(..))
where
import DDC.Type.Check.Judge.Kind
import DDC.Type.Check.Context
import DDC.Type.Check.Error
import DDC.Type.Check.ErrorMessage      ()
import DDC.Type.Check.CheckCon
import DDC.Type.Check.Config
import DDC.Type.Exp
import DDC.Base.Pretty
import DDC.Type.Pretty                   ()
import DDC.Type.Env                      (KindEnv)
import DDC.Control.Monad.Check           (evalCheck)
import qualified DDC.Type.Env            as Env


-- Wrappers -------------------------------------------------------------------
-- | Check a type in the given environment,
--   returning an error or its kind.
checkType  :: (Ord n, Show n, Pretty n) 
           => Config n 
           -> KindEnv n 
           -> Type n
           -> Either (Error n) (Type n, Kind n)

checkType defs env tt 
 = evalCheck () 
 $ do   (t, k, _)       <- checkTypeM defs env emptyContext tt
        return (t, k)


-- | Check a type in the given environment and local context,
--   returning an error or its kind.
checkTypeWithContext 
        :: (Ord n, Show n, Pretty n) 
        => Config n 
        -> KindEnv n 
        -> Context n
        -> Type n
        -> Either (Error n) (Type n, Kind n)

checkTypeWithContext defs env ctx tt 
 = evalCheck () 
 $ do   (t, k, _)       <- checkTypeM defs env ctx tt
        return (t, k)


-- | Check a type in an empty environment, returning an error or its kind.
kindOfType
        :: (Ord n, Show n, Pretty n) 
        => Config n
        -> Type n 
        -> Either (Error n) (Kind n)

kindOfType defs tt
 = evalCheck () 
 $ do   (_, k, _)       <- checkTypeM defs Env.empty emptyContext tt
        return k



