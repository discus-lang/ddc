
module DDC.Core.Tetra.Convert.Exp.Arg
        (convertOrDiscardSuperArgX)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Core.Check                   (AnTEC(..))
import qualified DDC.Core.Tetra.Prim    as E
import qualified DDC.Core.Salt.Name     as A
import qualified DDC.Core.Salt.Runtime  as A


---------------------------------------------------------------------------------------------------
-- | Given an argument to a function or data constructor, either convert
--   it to the corresponding argument to use in the Salt program, or 
--   return Nothing which indicates it should be discarded.
convertOrDiscardSuperArgX
        :: Context a                    -- ^ Type context of the conversion.
        -> ( Arg (AnTEC a E.Name) E.Name
           , Type E.Name)               -- ^ Argument to convert along with its type.
        -> ConvertM a (Maybe (Arg a A.Name))

convertOrDiscardSuperArgX ctx (aa, tArg)
 = case aa of

        -- Convert Type arguments.
        RType t

          -- In the salt code everything currently goes into the top-level region.
         | isRegionKind tArg
         -> do  return  $ Just $ RType A.rTop
 
         -- If we have a data type argument where the type is boxed,
         -- then we pass the region the corresponding Salt object is in.
         | isDataKind   tArg
         -> do  let kenv =  contextKindEnv ctx
                t'       <- saltPrimeRegionOfDataType kenv t
                return   $ Just $ RType t'

         -- Drop other type arguments.
         | otherwise
         ->     return Nothing


        -- Drop Witness arguments.
        RWitness{}
         ->     return Nothing


        -- Convert Term arguments.
        RTerm x
         -> do  x'      <- contextConvertExp ctx ExpArg ctx x
                return  $ Just $ RTerm x'

        RImplicit x
         -> do  x'      <- contextConvertExp ctx ExpArg ctx x
                return  $ Just $ RImplicit x'


---------------------------------------------------------------------------------------------------
-- [Note: Salt conversion for higher kinded type arguments]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Converting functions that use higher kinded types to Salt is problematic
-- because we can't directly see what region is being used to represent
-- each object.
--
--   data List (r : Region) (a : Data) where ...
--
--   idf [c : Data ~> Data] [a : Data] (x : c a) : Nat# ...
--
--   f = ... idf [List r1] [Nat] (...)
--
-- At the call-site, the value argument to idf is in region r1, but that
-- information is not available when converting the body of 'idf'.
-- When converting the body of 'idf' we can't assume the value bound to 
-- 'x' is in rTop.
--
-- We need some simple subtyping in region types, to have a DontKnow region
-- that can be used to indicate that the region an object is in is unknown.
--
-- For now we just don't convert functions using higher kinded types, 
-- and leave this to future work. Higher kinding isn't particularly 
-- useful without a type clasing system with constructor classes,
-- so we'll fix it later.
--
