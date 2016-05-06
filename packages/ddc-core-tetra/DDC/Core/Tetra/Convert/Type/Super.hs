
module DDC.Core.Tetra.Convert.Type.Super
        (convertSuperConsT)
where
import DDC.Core.Tetra.Convert.Type.Kind
import DDC.Core.Tetra.Convert.Type.Data
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Call
import DDC.Core.Exp.Annot.Exp
import DDC.Type.Exp.Simple
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Name             as A


-- | Convert the Tetra type of a super with the given call pattern to Salt.
--
--   This type conversion mirrors the `convertSuperXT` conversion function
--   except that we only know the call pattern of the function, rather than
--   its defining expression.
-- 
convertSuperConsT
        :: Context 
        -> [Cons E.Name]
        -> Type E.Name 
        -> ConvertM a (Type A.Name)

convertSuperConsT ctx0 cs0 tt0
 = convertAbsType ctx0 cs0 tt0
 where
        -- Accepting type abstractions --------------------
        convertAbsType ctx cs tt
         = case cs of
                ConsType _k : cs'
                  | TForall bParam tBody <- tt
                  -> convertConsType ctx bParam cs' tBody
                _ -> convertAbsValue ctx cs tt

        convertConsType ctx bParam cs tBody
         -- Erase higher kinded type abstractions.
         | Just _       <- takeKFun $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' cs tBody

         -- Erase effect abstractions.
         | isEffectKind $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' cs tBody

         -- Retain region abstractions.
         | isRegionKind $ typeOfBind bParam
         = do   bParam'  <- convertTypeB bParam
                let ctx' =  extendKindEnv bParam ctx
                tBody'   <- convertAbsType ctx' cs tBody
                return   $  TForall bParam' tBody'

         -- Convert data type abstractions to region abstractions.
         | isDataKind $ typeOfBind bParam
         , BName (E.NameVar str) _ <- bParam
         , str'          <- str ++ "$r"
         , bParam'       <- BName (A.NameVar str') kRegion
         = do   let ctx' =  extendKindEnv bParam ctx
                tBody'   <- convertAbsType ctx' cs tBody
                return   $  TForall bParam' tBody'

         -- Some other type abstraction we can't convert.
         | otherwise
         = error "ddc-core-tetra.convertSuperConsT: cannot convert type abstraction."


        -- Accepting value abstractions -------------------
        convertAbsValue ctx cs tt
         = case cs of
                ConsValue tParam : cs'
                  | Just (_tArg, tBody)  <- takeTFun tt
                  -> convertConsValue ctx tParam cs' tBody
                _ -> convertDataT ctx tt


        convertConsValue ctx tParam cs tBody
         = do   tParam'  <- convertDataT   ctx tParam
                tBody'   <- convertAbsValue ctx cs tBody
                return   $  tFun tParam' tBody'

