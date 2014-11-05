
module DDC.Core.Tetra.Convert.Exp.Lets
        (convertLets)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import DDC.Control.Monad.Check           (throw)
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Name      as A
import Control.Monad


-- | Convert some let-bindings to Salt.
convertLets
        :: Show a 
        => Context
        -> Lets (AnTEC a E.Name) E.Name -- ^ Expression to convert.
        -> ConvertM a (Lets a A.Name)

convertLets ctx lts
 = let  defs     = contextDataDefs   ctx
        kenv     = contextKindEnv    ctx
        convertX = contextConvertExp ctx
   in case lts of
        LRec bxs
         -> do  let ctx'     = extendsTypeEnv (map fst bxs) ctx
                let (bs, xs) = unzip bxs

                -- All the recursive bindings must be functional values, 
                -- so we use convertDataB here instead of convertValueB.
                bs'          <- mapM (convertDataB defs   kenv) bs                
                xs'          <- mapM (convertX     ExpFun ctx') xs
                return  $ LRec $ zip bs' xs'

        LLet b x1
         -> do  let ctx'     = extendTypeEnv b ctx
                b'           <- convertValueB defs kenv b
                x1'          <- convertX      ExpBind ctx' x1
                return  $ LLet b' x1'

        LPrivate b mt bs
         -> do  
                b'           <- mapM convertTypeB b
                let ctx'     = extendsKindEnv b ctx
                let kenv'    = contextKindEnv ctx'
                
                bs'          <- mapM (convertCapabilityB kenv') bs
                mt'          <- case mt of
                                 Nothing -> return Nothing
                                 Just t  -> liftM Just $ convertRegionT kenv t
                return  $ LPrivate b' mt' bs'
  
        LWithRegion{}
         ->     throw $ ErrorMalformed "Cannot convert LWithRegion construct."

