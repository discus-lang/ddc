
module DDC.Core.Tetra.Convert.Type.Region
        ( convertRegionT
        , saltPrimeRegionOfDataType)
where
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp.Annot.Exp
import DDC.Type.Env
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Type.Env                   as Env
import DDC.Base.Pretty
       

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
--   Boxed data types whose first parameter is a region, by convention that
--   region is the prime one.
--     List r1 a  =>  r1 
--
--   Boxed data types that do not have a region as the first parameter,
--   these are allocated into the top-level region.
--     Unit       => rTop
--     B# Nat#    => rTop
--     
--   Functions are also allocated into the top-level region.
--     a -> b     => rTop
--     forall ... => rTop
--
--   For completely parametric data types we use a region named after the
--   associated type variable.
--     a          => a$r
--
--   For types with an abstract constructor, we currently reject them outright.
--   There's no way to tell what region an object of such a type should be 
--   allocated into. In future we should add a supertype of regions, and treat
--   such objects as belong to the Any region.
--   See [Note: Salt conversion for higher kinded type arguments]
--     c a b      => ** NOTHING **
--   
--   Unboxed and index types don't refer to boxed objects, so they don't have
--   associated prime regions.
--     Nat#       => ** NOTHING **
--     U# Nat#    => ** NOTHING **
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
        = do    -- u'      <- convertTypeU u
                return  A.rTop

        -- Boxed data types without an attached primary region variable.
        -- This also covers the function case.
        | TCon _ : _           <- takeTApps tt
        = do    return  A.rTop

        -- Quantified types.
        | TForall{}     <- tt
        = do    return  A.rTop

        -- Completely parametric data types.
        | TVar u        <- tt
        , Just k        <- Env.lookup u kenv
        , isDataKind k
        = do    return  A.rTop

        | otherwise
        = throw $ ErrorMalformed       
                $ "Cannot take prime region from " ++ (renderIndent $ ppr tt)

