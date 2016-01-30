
module DDC.Core.Tetra.Convert.Type.Base
        ( Context (..)
        , extendKindEnv
        , extendsKindEnv

        , convertK
        , convertTypeB
        , convertBindNameM)
where
import DDC.Core.Tetra.Convert.Error
import DDC.Type.Exp
import DDC.Type.DataDef
import DDC.Base.Pretty
import DDC.Control.Monad.Check                  (throw)
import DDC.Type.Env                             (KindEnv)
import Control.Monad
import Data.Set                                 (Set)
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Name             as A


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


---------------------------------------------------------------------------------------------------
-- | Convert a kind from Core Tetra to Core Salt.
convertK :: Kind E.Name -> ConvertM a (Kind A.Name)
convertK kk
        | TCon (TyConKind kc) <- kk
        = return $ TCon (TyConKind kc)

        | otherwise
        = throw $ ErrorMalformed 
                $ "Invalid kind " ++ (renderIndent $ ppr kk)


-- | Convert a type binder.
--   These are formal type parameters.
convertTypeB    :: Bind E.Name -> ConvertM a (Bind A.Name)
convertTypeB bb
 = case bb of
        BNone k         -> liftM BNone  (convertK k)
        BAnon k         -> liftM BAnon  (convertK k)
        BName n k       -> liftM2 BName (convertBindNameM n) (convertK k)


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
