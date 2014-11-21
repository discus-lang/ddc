
module DDC.Core.Tetra.Convert.Type.Base
        ( Context (..)
        , extendKindEnv
        , extendsKindEnv)
where
import DDC.Type.Exp
import DDC.Type.DataDef
import qualified DDC.Type.Env                           as Env
import qualified DDC.Core.Tetra.Prim                    as E
import DDC.Type.Env                                     (KindEnv)


-- Context  ---------------------------------------------------------------------------------------
-- | Context of a type conversion.
--   TODO: add set of foreign boxed data names.
data Context
        = Context
        { contextDefs           :: DataDefs E.Name       
        , contextKindEnv        :: KindEnv  E.Name }


extendKindEnv  ::  Bind E.Name  -> Context -> Context
extendKindEnv b ctx
        = ctx { contextKindEnv = Env.extend b (contextKindEnv ctx) }

extendsKindEnv :: [Bind E.Name] -> Context -> Context
extendsKindEnv bs ctx
        = ctx { contextKindEnv = Env.extends bs (contextKindEnv ctx) }


