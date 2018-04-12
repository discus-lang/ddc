{-# LANGUAGE CPP #-}

module DDC.Core.Discus.Convert.Exp.Lets
        (convertLets)
where
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Type
import DDC.Core.Discus.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Core.Check                                   (AnTEC(..))
import qualified DDC.Core.Discus.Convert.Type.Base      as T
import qualified DDC.Core.Discus.Prim                   as E
import qualified DDC.Core.Salt.Name                     as A
import qualified Data.Map                               as Map
import qualified Data.Text                              as T

#if __GLASGOW_HASKELL__ >= 741
import DDC.Data.Monoidal        ()
#else
import DDC.Data.Monoidal
#endif


-- | Convert some let-bindings to Salt.
convertLets
        :: Context a
        -> Lets (AnTEC a E.Name) E.Name -- ^ Expression to convert.
        -> ConvertM a (Maybe (Lets a A.Name), Context a)

convertLets ctx lts
 = let  convertX = contextConvertExp ctx
   in case lts of
        -- Recursive let-binding.
        LRec bxs
         -> do  let ctx'     = extendsTypeEnv (map fst bxs) ctx
                bxs'    <- mapM (uncurry (convertBinding ctx)) bxs
                return  ( Just $ LRec bxs'
                        , ctx')

        --  Polymorphic instantiation of a top-level super.
        --  See [Note: Binding top-level supers]
        LLet (BName nBind _) (XApp _ xa xb)
         | (xF, asArgs) <- takeXApps1 xa xb

         , tsArgs       <- [t | RType t <- asArgs]
         , (ksArgs, _)  <- takeTFunAllArgResult (annotType $ annotOfExp xF)

         , length tsArgs > 0
         , length tsArgs == length asArgs
         , XVar _ (UName nSuper)     <- xF
         , Map.member nSuper (contextCallable ctx)
         ->     return  ( Nothing
                        , ctx { contextSuperBinds
                                 = Map.insert nBind (nSuper, zip tsArgs ksArgs)
                                                    (contextSuperBinds ctx) })

        -- Standard non-recursive let-binding.
        LLet b x1
         -> do  b'      <- convertDataB (typeContext ctx) b
                x1'     <- convertX      ExpBind ctx x1
                return  ( Just $ LLet b' x1'
                        , extendTypeEnv b ctx)

        LPrivate bs _ _
         ->     return  ( Nothing
                        , extendsTypeEnv bs ctx)


-- | Convert a possibly recursive let binding.
convertBinding
        :: Context a
        -> Bind  E.Name
        -> Exp (AnTEC a E.Name) E.Name
        -> ConvertM a (Bind A.Name, Exp a A.Name)

convertBinding ctx b xx
 = do
        (x', t') <- convertSuperXT ctx xx (typeOfBind b)
        b'       <- case b of
                        BNone _   -> BNone <$> pure t'
                        BAnon _   -> BAnon <$> pure t'
                        BName n _ -> BName <$> convertBindNameM n <*> pure t'

        return  (b', x')


-- | Convert a supercombinator expression in parallel with its type.
--
--   This also checks that it is in the standard form,
--   meaning that type abstractions must be out the front,
--   then value abstractions, then the body expression.
--
convertSuperXT
        :: Context a
        -> Exp (AnTEC a E.Name) E.Name
        -> Type E.Name
        -> ConvertM a (Exp a A.Name, Type A.Name)

convertSuperXT    ctx0 xx0 tt0
 = convertAbsType ctx0 xx0 (typeContext ctx0) tt0
 where
        -- Accepting type abstractions --------------------
        convertAbsType ctxX xx ctxT tt
         = case xx of
                XLAM a bParamX xBody
                  |  TForall bParamT tBody    <- tt
                  -> convertXLAM a   ctxX bParamX xBody
                                     ctxT bParamT tBody

                _ -> convertAbsValue ctxX xx
                                     ctxT tt

        convertXLAM a ctxX bParamX xBody
                      ctxT bParamT tBody

         -- Erase higher kinded type abstractions.
         | Just _       <- takeKFun $ typeOfBind bParamX
         = do   let ctxX' =   extendKindEnv bParamX ctxX
                let ctxT' = T.extendKindEnv bParamT ctxT
                convertAbsType ctxX' xBody ctxT' tBody

         -- Erase effect abstractions.
         | isEffectKind $ typeOfBind bParamX
         = do   let ctxX' =   extendKindEnv bParamX ctxX
                let ctxT' = T.extendKindEnv bParamT ctxT
                convertAbsType ctxX' xBody ctxT' tBody

         -- Retain region abstractions.
         | isRegionKind $ typeOfBind bParamX
         = do   let a'    =  annotTail    a

                bParamX'  <- convertTypeB bParamX
                bParamT'  <- convertTypeB bParamT

                let ctxX' =   extendKindEnv bParamX ctxX
                let ctxT' = T.extendKindEnv bParamT ctxT

                (xBody', tBody')
                          <- convertAbsType ctxX' xBody ctxT' tBody

                return  ( XLAM a' bParamX' xBody'
                        , TForall bParamT' tBody')

         -- When a function is polymorphic in some boxed data type,
         -- then the type lambda in Discus is converted to a region
         -- lambda in Salt which binds the region the object is in.
         | isDataKind $ typeOfBind bParamX

         , BName nX _   <- bParamX
         , Just strX    <- takeNameStr nX
         , strX'        <- strX <> T.pack "$r"
         , bParamX'     <- BName (A.NameVar strX') kRegion

         , BName nT _   <- bParamT
         , Just strT    <- takeNameStr nT
         , strT'        <- strT <> T.pack "$r"
         , bParamT'     <- BName (A.NameVar strT') kRegion

         = do   let a'    =  annotTail a

                let ctxX' =   extendKindEnv bParamX ctxX
                let ctxT' = T.extendKindEnv bParamT ctxT

                (xBody', tBody')
                         <- convertAbsType ctxX' xBody ctxT' tBody

                return  ( XLAM a' bParamX' xBody'
                        , TForall bParamT' tBody')

         -- Cannot convert this type abstraction.
         -- Maybe the binder is anonymous.
         | otherwise
         = error "ddc-core-discus.convertSuperXLAM: Cannot convert type abstraction."


        -- Accepting value abstractions -------------------
        convertAbsValue ctxX xx ctxT tt
         = case xx of
                XLam a bParamX xBody
                  |  Just (tParamT, tBody)  <- takeTFun tt
                  -> convertXLam a ctxX bParamX xBody
                                   ctxT tParamT tBody

                _ -> convertBody ctxX xx ctxT tt


        convertXLam a ctxX bParamX xBody
                      ctxT tParamT tBody
         = do
                let a'      = annotTail a

                let ctxX'   = extendTypeEnv bParamX ctxX

                bParamX'   <- convertDataB (typeContext ctxX) bParamX
                tParamT'   <- convertDataT ctxT tParamT

                (xBody', tBody') <- convertAbsValue ctxX' xBody ctxT tBody

                return  ( XLam a' bParamX' xBody'
                        , tFun tParamT' tBody')


        -- Converting body expressions---------------------
        convertBody ctxX xx ctxT tt
         = do   xBody'  <- contextConvertExp ctxX ExpBody ctxX xx
                tBody'  <- convertDataT ctxT tt
                return  ( xBody', tBody' )


takeNameStr (E.NameVar str)
                = Just $ str

takeNameStr (E.NameExt (E.NameVar str1) str2)
                = Just $ str1 <> T.pack "$" <> str2

takeNameStr _   = Nothing

-- Note: Binding top-level supers.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- After the Curry transform completes, we can still have local bindings like
-- 'f = g [r]', where 'g' is some top-level super. However, we can't bind the
-- names of top-level supers in Salt.
--
-- When generating code for higher order functions, there will be probably be
-- a 'creify# f' call later on. As the Salt-level reify operation only works
-- on the names of top-level supers rather than local bindings, remember that
-- 'f' is just an instantiation of 'g' so when we find the 'creify# f' we can
-- point it to 'g' instead.
--
-- This fakes up enough binding of functional values to make code generation
-- easy, but they're still not first class. We cannot pass or return functional
-- values to/from other functions.
--

