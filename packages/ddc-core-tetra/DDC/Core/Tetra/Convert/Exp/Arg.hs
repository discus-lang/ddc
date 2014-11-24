
module DDC.Core.Tetra.Convert.Exp.Arg
        (convertOrDiscardSuperArgX)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Name      as A

import DDC.Base.Pretty
import DDC.Control.Monad.Check           (throw)


---------------------------------------------------------------------------------------------------
-- | Given an argument to a function or data constructor, either convert
--   it to the corresponding argument to use in the Salt program, or 
--   return Nothing which indicates it should be discarded.
convertOrDiscardSuperArgX
        :: Show a                       
        => Exp (AnTEC a E.Name) E.Name  -- ^ Overall application expression, for debugging.
        -> Context a
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Maybe (Exp a A.Name))

convertOrDiscardSuperArgX xxApp ctx xx

        -- Region type arguments get passed through directly.
        | XType a t     <- xx
        , isRegionKind (annotType a)
        = do    t'       <- convertRegionT (typeContext ctx) t
                return   $ Just (XType (annotTail a) t')

        -- If we have a data type argument where the type is boxed, then we pass
        -- the region the corresponding Salt object is in.
        | XType a t     <- xx
        , isDataKind   (annotType a)
        = do    let kenv =  contextKindEnv ctx
                t'       <- saltPrimeRegionOfDataType kenv t
                return   $ Just (XType (annotTail a) t')

        -- Drop effect arguments.
        | XType a _     <- xx
        , isEffectKind (annotType a)
        =       return Nothing

        -- Some type that we don't know how to convert to Salt.
        -- We don't handle type args with higher kinds.
        -- See [Note: Salt conversion for higher kinded type arguments]
        | XType{}       <- xx
        = throw $ ErrorUnsupported xx
        $ vcat [ text "Unsupported type argument to function or constructor."
               , text "In particular, we don't yet handle higher kinded type arguments."
               , empty
               , text "See [Note: Salt conversion for higher kinded type arguments] in"
               , text "the implementation of the Tetra to Salt conversion." 
               , empty
               , text "with application: " <+> ppr xxApp ]

        -- Witness arguments are discarded.
        | XWitness{}    <- xx
        =       return  $ Nothing

        -- Expression arguments.
        | otherwise
        = do    x'      <- contextConvertExp ctx ExpArg ctx xx
                return  $ Just x'


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
