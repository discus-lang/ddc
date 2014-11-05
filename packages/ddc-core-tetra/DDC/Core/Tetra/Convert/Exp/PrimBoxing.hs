
module DDC.Core.Tetra.Convert.Exp.PrimBoxing
        (convertPrimBoxing)
where
import DDC.Core.Tetra.Convert.Exp.Lit
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Data
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error

import DDC.Core.Transform.LiftX
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A


-- | Convert a Tetra boxing primop to Salt.
convertPrimBoxing
        :: Show a 
        => ExpContext                   -- ^ The surrounding expression context.
        -> Context                      -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimBoxing _ectx ctx xx
 = let  pp        = contextPlatform ctx
        defs      = contextDataDefs ctx
        kenv      = contextKindEnv  ctx
        tenv      = contextTypeEnv  ctx
 
        convertX  = contextConvertExp  ctx
        downArgX  = convertX           ExpArg ctx 

   in case xx of

        ---------------------------------------------------
        -- Wrapping of pure values into boxed values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tBIx, XType _ tBx, XCon _ c]) <- takeXPrimApps xx
         , isBoxableIndexType tBIx
         , isBoxedRepType     tBx
         , Just dt      <- makeDataTypeForBoxableIndexType tBIx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> Just $ do  
                let a'  = annotTail a
                xArg'   <- convertLitCtor a' c
                tBIx'   <- convertIndexT tBIx

                constructData pp kenv tenv a'
                        dt dc A.rTop [xArg'] [tBIx']


        ---------------------------------------------------
        -- Unwrapping of boxed values into pure values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tBx, XType _ tBIx, xArg])    <- takeXPrimApps xx
         , isBoxedRepType     tBx
         , isBoxableIndexType tBIx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> Just $ do  
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tBIx'   <- convertIndexT   tBIx
                tBx'    <- convertValueT defs kenv tBx

                x'      <- destructData pp a' dc
                                (UIx 0) A.rTop 
                                [BAnon tBIx'] (XVar a' (UIx 0))

                return  $ XLet a' (LLet (BAnon tBx') (liftX 1 xArg'))
                                  x'

        ---------------------------------------------------
        -- Boxing of unboxed values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for user-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tUx, XType _ tBx, xArg])     <- takeXPrimApps xx
         , isUnboxedRepType tUx
         , isBoxedRepType   tBx
         , Just tBIx    <- takeIndexOfBoxedRepType tBx
         , Just dt      <- makeDataTypeForBoxableIndexType tBIx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> Just $ do  
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tBIx'   <- convertIndexT tBIx

                constructData pp kenv tenv a'
                        dt dc A.rTop [xArg'] [tBIx']


        ---------------------------------------------------
        -- Unboxing of boxed values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tBx, XType _ tUx, xArg])     <- takeXPrimApps xx
         , isBoxedRepType   tBx
         , isUnboxedRepType tUx
         , Just tBIx    <- takeIndexOfBoxedRepType tBx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> Just $ do
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tBIx'   <- convertIndexT   tBIx
                tBx'    <- convertValueT defs kenv tBx

                x'      <- destructData pp a' dc
                                (UIx 0) A.rTop 
                                [BAnon tBIx'] (XVar a' (UIx 0))

                return  $ XLet a' (LLet (BAnon tBx') (liftX 1 xArg'))
                                  x'

        ---------------------------------------------------
        -- This isn't a boxing primitive.
        _ -> Nothing

