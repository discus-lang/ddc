
module DDC.Core.Discus.Convert.Exp.PrimBoxing
        (convertPrimBoxing)
where
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Boxing
import DDC.Core.Discus.Convert.Data
import DDC.Core.Discus.Convert.Type
import DDC.Core.Discus.Convert.Error

import DDC.Core.Transform.BoundX
import DDC.Core.Exp.Annot
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Discus.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A


-- | Convert a Discus boxing primop to Salt.
convertPrimBoxing
        :: Show a
        => ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimBoxing _ectx ctx xx
 = let  pp        = contextPlatform ctx
        convertX  = contextConvertExp  ctx
        downArgX  = convertX           ExpArg ctx

   in case xx of

        -- Boxing of unboxed numeric values.
        --   The unboxed representation of a numeric value is the machine value.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for user-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert False
                , [RType tUx, RType tBx, RTerm xArg])   <- takeXFragApps xx
         , isUnboxedRepType tUx
         , isNumericType    tBx
         , Just dc      <- makeBoxedPrimDataCtor tBx
         -> Just $ do
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tUx'    <- convertDataPrimitiveT tBx

                constructData pp a'
                        dc A.rTop [xArg'] [tUx']


        -- Unboxing of boxed values.
        --   The unboxed representation of a numeric value is the machine value.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert False
                , [RType tBx, RType tUx, RTerm xArg])   <- takeXFragApps xx
         , isUnboxedRepType tUx
         , isNumericType    tBx
         , Just dc      <- makeBoxedPrimDataCtor tBx
         -> Just $ do
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tBx'    <- convertDataT (typeContext ctx) tBx
                tUx'    <- convertDataPrimitiveT tBx

                x'      <- destructData pp a' dc
                                (UIx 0) A.rTop
                                [BAnon tUx'] (XVar a' (UIx 0))

                return  $ XLet a' (LLet (BAnon tBx') (liftX 1 xArg')) x'

        -- This isn't a boxing primitive.
        _ -> Nothing

