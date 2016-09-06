
module DDC.Core.Tetra.Convert.Type.Region
        ( convertRegionT
        , saltPrimeRegionOfDataType)
where
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp.Annot.Exp
import DDC.Type.Env
import DDC.Type.Exp.Simple
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Type.Env                   as Env
import DDC.Data.Pretty
       

-- Region Types -----------------------------------------------------------------------------------
-- | Convert a region type to Salt.
convertRegionT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertRegionT ctx tt
        | TVar u        <- tt
        , Just k        <- Env.lookup u (contextKindEnv ctx)
        , isRegionKind k
        = return $ A.rTop

        | otherwise
        = throw  $ ErrorMalformed 
                 $ "Invalid region type " ++ (renderIndent $ ppr tt)


-- Prime Region -----------------------------------------------------------------------------------
-- | Given the type of some data value, determine what prime region to use 
--   for the object in the Salt language. The supplied type must have kind
--   Data, else you'll get a bogus result.
-- 
--   NOTE: This used to do something more useful, but was changed so that
--   all values go into rTop. The problem is that given a type like (m Nat)
--   we can't know what region the value will be allocated into as the
--   type is too polymorphic. We need to implement a region subtyping system
--   so that the Top region can be treated as an Any/DontKnow specifier
--   rather than a specific region that is distinct from all others.
--   
saltPrimeRegionOfDataType
        :: KindEnv E.Name 
        -> Type E.Name 
        -> ConvertM a (Type A.Name)

saltPrimeRegionOfDataType kenv tt
        -- Boxed data types with an attached primary region variable.
        | TCon _ : TVar u : _   <- takeTApps tt
        , Just k                <- Env.lookup u kenv
        , isRegionKind k
        = do    return  A.rTop

        -- Completely parametric data types.
        | TVar u        <- tt
        , Just k        <- Env.lookup u kenv
        , isDataKind k
        = do    return  A.rTop

        -- Boxed data types without an attached primary region variable.
        --   For applications of abstract type constructors like in 
        --   (m Nat), we can't know what constructor 'm' will be instantiated
        --   with, nor what region the resulting value will be allocated into.
        | TCon{}  <- tt
        = do    return  A.rTop

        | TApp{}   <- tt
        = do    return  A.rTop

        -- Quantified types.
        | TForall{}     <- tt
        = do    return  A.rTop

        -- Type variable which is not in the environment.
        | otherwise
        = throw $ ErrorMalformed       
                $ "Cannot take prime region from " ++ (renderIndent $ ppr tt)

