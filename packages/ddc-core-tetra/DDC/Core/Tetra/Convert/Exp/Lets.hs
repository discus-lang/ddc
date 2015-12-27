
module DDC.Core.Tetra.Convert.Exp.Lets
        (convertLets)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                   (AnTEC(..))
import qualified DDC.Core.Tetra.Prim    as E
import qualified DDC.Core.Salt.Name     as A
import qualified Data.Map               as Map


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
                let (bs, xs) = unzip bxs

                -- All the recursive bindings must be functional values, 
                -- so we use convertDataB here instead of convertValueB.
                bs'          <- mapM (convertSuperB (typeContext ctx)) bs       
                                            -- TODO: don't assume all letrecs bind supers
                xs'          <- mapM (convertX      ExpFun ctx') xs
                return  ( Just $ LRec $ zip bs' xs'
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
        LLet b x1
         -> do  b'      <- convertValueB (typeContext ctx) b
                x1'     <- convertX      ExpBind ctx x1
                return  ( Just $ LLet b' x1'
                        , extendTypeEnv b ctx)

        -- Introduce a private region.
{-
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
         ->     return  (Nothing
                        , extendsTypeEnv bs ctx)


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



