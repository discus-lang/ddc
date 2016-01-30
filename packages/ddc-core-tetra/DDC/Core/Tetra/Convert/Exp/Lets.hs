
module DDC.Core.Tetra.Convert.Exp.Lets
        (convertLets)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Type.Predicates
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                                   (AnTEC(..))
import qualified DDC.Core.Tetra.Prim                    as E
import qualified DDC.Core.Salt.Name                     as A
import qualified Data.Map                               as Map
        

-- | Convert some let-bindings to Salt.
convertLets
        :: Show a  
        => Context a
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
         | (xF, xsArgs) <- takeXApps1 xa xb
         , atsArgs      <- [(a, t) | XType a t <- xsArgs]
         , tsArgs       <- map snd atsArgs
         , length tsArgs > 0
         , length xsArgs == length tsArgs
         , XVar _ (UName nSuper)     <- xF
         , Map.member nSuper (contextCallable ctx)
         ->     return  ( Nothing
                        , ctx { contextSuperBinds
                                 = Map.insert nBind (nSuper, atsArgs) (contextSuperBinds ctx) })

        -- Standard non-recursive let-binding.
        -- TODO: Do via convertBinding.
        LLet b x1
         -> do  b'      <- convertValueB (typeContext ctx) b
                x1'     <- convertX      ExpBind ctx x1
                return  ( Just $ LLet b' x1'
                        , extendTypeEnv b ctx)

        -- Introduce a private region.
{-      -- TODO: we need these for Disctinct witnesses.
        LPrivate b mt bs
         -> do  
                b'  <- mapM convertTypeB b
                let ctx' = extendsKindEnv b ctx
                
                bs' <- mapM (convertCapabilityB (typeContext ctx')) bs
                mt' <- case mt of
                        Nothing -> return Nothing
                        Just t  -> liftM Just $ convertRegionT (typeContext ctx) t

                return  ( Just $ LPrivate b' mt' bs'
                        , extendsTypeEnv bs $ extendsKindEnv b ctx)
  -}
        LPrivate bs _ _
         ->     return  ( Nothing
                        , extendsTypeEnv bs ctx)


-- | Convert a possibly recursive let binding.
---
--   TODO: Don't assume all letrecs bind lambda abstractions.
convertBinding
        :: Show a
        => Context a
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


-- | Convert a supercombinator expression and type.
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
 = convertAbsType ctx0 xx0 tt0
 where
        -- Accepting type abstractions --------------------
        convertAbsType ctx xx tt
         = case xx of
                -- TODO: check kinds.
                XLAM a bParam xBody
                  |  TForall _bParam' tBody    <- tt
                  -> convertXLAM ctx a bParam xBody tBody 

                _ -> convertAbsValue ctx xx tt


        convertXLAM ctx a bParam xBody tBody 
         -- Erase higher kinded type abstractions.
         | Just _       <- takeKFun $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' xBody tBody

         -- Erase effect abstractions.
         | isEffectKind $ typeOfBind bParam
         = do   let ctx' = extendKindEnv bParam ctx
                convertAbsType ctx' xBody tBody

         -- Retain region abstractions.
         | isRegionKind $ typeOfBind bParam
         = do   let a'    =  annotTail    a
                bParam'   <- convertTypeB bParam

                let ctx'  =  extendKindEnv bParam ctx
                (xBody', tBody')    
                          <- convertAbsType ctx' xBody tBody

                -- TODO: bParam / bParam' mismatch.
                -- We're converting the type in parallel with the expression,
                -- and the binders may have different names.
                return  ( XLAM a' bParam' xBody'                
                        , TForall bParam' tBody')

         -- When a function is polymorphic in some boxed data type,
         -- then the type lambda in Tetra is converted to a region lambda in
         -- Salt which binds the region the object is in.
         | isDataKind $ typeOfBind bParam
         , BName (E.NameVar str) _ <- bParam
         , str'         <-  str ++ "$r"
         , bParam'      <-  BName (A.NameVar str') kRegion
         = do   let a'   =  annotTail a
                let ctx' =  extendKindEnv bParam ctx 
                (xBody', tBody')   
                         <- convertAbsType ctx' xBody tBody

                return  ( XLAM a' bParam' xBody'
                        , TForall bParam' tBody')

         -- Convert the body of the function.
         | otherwise
         = error "convertSuperXLAM: nope"


        -- Accepting value abstractions -------------------
        convertAbsValue ctx xx tt
         = case xx of
                -- TODO: check types.
                XLam a bParam xBody
                  |  Just (_tArg, tBody)  <- takeTFun tt
                  -> convertXLam ctx a bParam xBody tBody

                _ -> convertBody ctx xx tt


        convertXLam ctx a bParam xBody tBody
         = do   let ctx'    = extendTypeEnv bParam ctx
                let a'      = annotTail a
                bParam'    <- convertValueB (typeContext ctx) bParam
                tParam'    <- convertValueT (typeContext ctx) (typeOfBind bParam)

                (xBody', tBody') <- convertAbsValue ctx' xBody tBody

                return  ( XLam a' bParam' xBody'
                        , tFun tParam' tBody')


        -- Converting body expressions---------------------
        convertBody ctx xx tt
         = do   xBody'  <- contextConvertExp ctx ExpBody ctx xx
                tBody'  <- convertValueT (typeContext ctx) tt
                return  ( xBody', tBody' )



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

