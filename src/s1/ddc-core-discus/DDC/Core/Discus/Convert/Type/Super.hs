{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.Discus.Convert.Type.Super
        ( convertSuperT
        , convertSuperConsT)
where
import DDC.Core.Discus.Convert.Type.Kind
import DDC.Core.Discus.Convert.Type.Data
import DDC.Core.Discus.Convert.Type.Base
import DDC.Core.Discus.Convert.Error
import DDC.Core.Call
import DDC.Core.Exp.Annot
import qualified DDC.Core.Discus.Prim   as E
import qualified DDC.Core.Salt.Name     as A
import Data.Monoid

---------------------------------------------------------------------------------------------------
-- | Convert the type of a super to Salt.
--
--   This function is used to convert the types of data constructors as well as
--   the types of foreign imported functions. We assume that the implementation
--   takes all arguments directly, so the arity of the underling super is the
--   same as the number of arguments in its type.
--
--   TODO: look through type synonyms during conversion.
--
convertSuperT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertSuperT ctx0 tt0
 = convertAbsType ctx0 tt0
 where
        -- Accepting type abstractions --------------------
        convertAbsType ctx tt
         = case tt of
                TForall bParam tBody
                  -> convertConsType ctx bParam tBody
                _ -> convertAbsValue ctx tt

        convertConsType ctx bParam tBody
         -- Erase higher kinded type abstractions.
         | Just _       <- takeKFun $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' tBody

         -- Erase effect abstractions.
         | isEffectKind $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' tBody

         -- Retain region abstractions.
         | isRegionKind $ typeOfBind bParam
         = do   bParam' <- convertTypeB  bParam
                let ctx' = extendKindEnv bParam ctx
                tBody'  <- convertSuperT ctx' tBody
                return  $ TForall bParam' tBody'

         -- Convert data type abstractions to region abstractions.
         | isDataKind   $ typeOfBind bParam
         , BName (E.NameVar str) _   <- bParam
         , str'         <- str <> "$r"
         , bParam'      <- BName (A.NameVar str') kRegion
         = do   let ctx' = extendKindEnv bParam ctx
                tBody'  <- convertAbsType ctx' tBody
                return  $ TForall bParam' tBody'

         -- Some other type that we can't convert.
         | otherwise
         = error "ddc-core-discus.converCtorT: cannot convert type."


        -- Accepting term abstractions -------------------
        convertAbsValue ctx tt
         = case tt of
                TApp{}
                 -- Suspension types in the body of foreign functions tell
                 -- us what the type of the effect is, but the function
                 -- itself does not produce a suspension value.
                 | Just (_tEff, tResult) <- takeTSusp tt
                 -> convertDataT ctx tResult

                 | Just (tParam, tBody) <- takeTFun tt
                 -> convertConsValue ctx tParam tBody

                _ -> convertDataT ctx tt


        convertConsValue ctx tParam tBody
         = do   tParam' <- convertDataT    ctx tParam
                tBody'  <- convertAbsValue ctx tBody
                return  $  tFun tParam' tBody'


-------------------------------------------------------------------------------
-- | Convert the Discus type of a super with the given call pattern to Salt.
--
--   This type conversion mirrors the `convertSuperXT` conversion function
--   except that we only know the call pattern of the function, rather than
--   its defining expression.
--
--   TODO: look through type synonyms during conversion.
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
         , str'          <- str <> "$r"
         , bParam'       <- BName (A.NameVar str') kRegion
         = do   let ctx' =  extendKindEnv bParam ctx
                tBody'   <- convertAbsType ctx' cs tBody
                return   $  TForall bParam' tBody'

         -- Some other type abstraction we can't convert.
         | otherwise
         = error "ddc-core-discus.convertSuperConsT: cannot convert type abs."


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

