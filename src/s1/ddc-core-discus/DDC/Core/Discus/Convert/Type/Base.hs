
module DDC.Core.Discus.Convert.Type.Base
        ( Context (..)
        , extendKindEnv
        , extendsKindEnv
        , convertBindNameM)
where
import DDC.Core.Discus.Convert.Error
import DDC.Type.Exp
import DDC.Type.DataDef
import DDC.Control.Check                        (throw)
import DDC.Type.Env                             (KindEnv)
import Data.Set                                 (Set)
import Data.Map.Strict                          (Map)
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Discus.Prim            as E
import qualified DDC.Core.Salt.Name             as A


-- | Context of a type conversion.
data Context
        = Context
        { -- | Data type definitions.
          --   These are all the visible data type definitions, from both
          --   the current module and imported ones.
          contextDataDefs       :: DataDefs E.Name

          -- | Type equations.
        , contextTypeEqns       :: Map E.Name (Type E.Name)

          -- | Names of foreign boxed data type contructors.
          --   These are names like 'Ref' and 'Array' that are defined in the
          --   runtime system rather than as an algebraic data type with a
          --   Discus-level data type definition. Although there is no data
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


-- | Convert the name of a Bind.
convertBindNameM :: E.Name -> ConvertM a A.Name
convertBindNameM nn
 = case nn of
        E.NameVar str
          -> return $ A.NameVar str

        E.NameExt n str
          -> do  n'      <- convertBindNameM n
                 return  $ A.NameExt n' str

        _ -> throw $ ErrorInvalidBinder nn
