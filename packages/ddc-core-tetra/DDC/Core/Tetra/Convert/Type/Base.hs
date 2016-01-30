
module DDC.Core.Tetra.Convert.Type.Base
        ( Context (..)
        , extendKindEnv
        , extendsKindEnv)
where
import DDC.Type.Exp
import DDC.Type.DataDef
import DDC.Type.Env                             (KindEnv)
import Data.Set                                 (Set)
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Tetra.Prim            as E


-- Context  ---------------------------------------------------------------------------------------
-- | Context of a type conversion.
data Context
        = Context
        { -- | Data type definitions.
          --   These are all the visible data type definitions, from both
          --   the current module and imported ones.
          contextDataDefs       :: DataDefs E.Name       

          -- | Names of foreign boxed data type contructors.
          --   These are names like 'Ref' and 'Array' that are defined in the
          --   runtime system rather than as an algebraic data type with a 
          --   Tetra-level data type definition. Although there is no data
          --   type definition, we still represent the values of these types
          --   in generic boxed form.
        , contextForeignBoxedTypeCtors 
                                :: Set      E.Name

        , contextKindEnv        :: KindEnv  E.Name }


extendKindEnv  ::  Bind E.Name  -> Context -> Context
extendKindEnv b ctx
        = ctx { contextKindEnv = Env.extend b (contextKindEnv ctx) }

extendsKindEnv :: [Bind E.Name] -> Context -> Context
extendsKindEnv bs ctx
        = ctx { contextKindEnv = Env.extends bs (contextKindEnv ctx) }


