
module DDC.Core.Salt.Convert.Type
        ( convTypeM
        , convFunctionTypeM)
where
import DDC.Core.Salt.Convert.Prim
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Name
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Module                          as C
import DDC.Core.Exp
import DDC.Type.Env                             (KindEnv)
import DDC.Base.Pretty
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Type.Env                   as Env


-- Type -----------------------------------------------------------------------
-- | Convert a Salt type to C source text.
--   This is used to convert the types of function parameters and locally
--   defined variables. It only handles non-function types, as we don't
--   directly support higher-order functions or partial application in Salt.
convTypeM :: KindEnv Name -> Type Name -> ConvertM a Doc
convTypeM kenv tt
 = case tt of
        TVar u
         -> case Env.lookup u kenv of
             Nothing            
              -> throw $ ErrorUndefined u

             Just k
              | isDataKind k -> return $ text "Obj*"
              | otherwise    
              -> throw $ ErrorTypeInvalid tt

        TCon{}
         | TCon (TyConBound (UPrim (NamePrimTyCon tc) _) _)      <- tt
         , Just doc     <- convPrimTyCon tc
         -> return doc

         | TCon (TyConBound (UPrim NameObjTyCon _) _)   <- tt
         -> return  $ text "Obj"


        TApp{}
         | Just (NamePrimTyCon PrimTyConPtr, [_, t2])   <- takePrimTyConApps tt
         -> do  t2'     <- convTypeM kenv t2
                return  $ t2' <> text "*"

        TForall b t
          -> convTypeM (Env.extend b kenv) t

        _ -> throw $ ErrorTypeInvalid tt


-- | Convert a Salt function type to a C source prototype.
convFunctionTypeM
        :: KindEnv  Name
        -> QualName Name        -- ^ Function name.
        -> Type     Name        -- ^ Function type.
        -> ConvertM a Doc

convFunctionTypeM kenv nFunc tFunc
 | TForall b t' <- tFunc
 = convFunctionTypeM (Env.extend b kenv) nFunc t'

 | otherwise
 = do   -- Qualifiers aren't supported yet.
        let QualName _ n = nFunc        
        let nFun'        = text $ sanitizeGlobal (renderPlain $ ppr n)

        let (tsArgs, tResult) = takeTFunArgResult tFunc

        tsArgs'          <- mapM (convTypeM kenv) tsArgs
        tResult'         <- convTypeM kenv tResult

        return $ tResult' <+> nFun' <+> parenss tsArgs'


parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs
