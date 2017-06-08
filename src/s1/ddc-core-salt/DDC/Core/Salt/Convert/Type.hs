
module DDC.Core.Salt.Convert.Type
        ( convTypeM
        , convSuperTypeM)
where
import DDC.Core.Salt.Convert.Prim
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Convert.Name
import DDC.Core.Salt.Name
import DDC.Core.Module                          as C
import DDC.Core.Exp.Annot
import DDC.Type.Env                             (KindEnv)
import DDC.Data.Pretty
import DDC.Control.Check                        (throw)
import qualified DDC.Type.Env                   as Env


-- Type -------------------------------------------------------------------------------------------
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
         | TCon (TyConSpec TcConUnit)                       <- tt
         -> return  $ text "Obj*"


         | TCon (TyConBound (UPrim (NamePrimTyCon tc) _) _) <- tt
         , Just doc     <- convPrimTyCon tc
         -> return doc

         | TCon (TyConBound (UPrim NameObjTyCon _) _)       <- tt
         -> return  $ text "Obj"

        TApp{}
         | Just (NamePrimTyCon PrimTyConPtr, [_, t2])   <- takePrimTyConApps tt
         -> do  t2'     <- convTypeM kenv t2
                return  $ t2' <> text "*"

        TForall b t
          -> convTypeM (Env.extend b kenv) t

        _ -> throw $ ErrorTypeInvalid tt


---------------------------------------------------------------------------------------------------
-- | Convert a Salt function type to a C source prototype.
convSuperTypeM
        :: KindEnv      Name
        -> Maybe (ImportValue  Name (Type Name))
        -> Maybe (ExportSource Name (Type Name))
        -> Name                 -- ^ Local name of super.
        -> Type         Name    -- ^ Function type.
        -> ConvertM a Doc

convSuperTypeM kenv misrc mesrc nSuper tSuper
 | TForall b t' <- tSuper
 = convSuperTypeM (Env.extend b kenv) misrc mesrc nSuper t'

 | Just nFun'   <- seaNameOfSuper misrc mesrc nSuper
 = do
        -- Convert the argument and return types.
        let (tsArgs, tResult) = takeTFunArgResult tSuper
        tsArgs'         <- mapM (convTypeM kenv) $ filter keepParamOfType tsArgs
        tResult'        <- convTypeM kenv tResult

        return $ tResult' <+> nFun' <+> parenss tsArgs'

 | otherwise
 = throw $ ErrorImportInvalid nSuper

    
keepParamOfType :: Type Name -> Bool
keepParamOfType tt
 | tc : _       <- takeTApps tt
 , isWitnessType tc
                = False

 | otherwise    = True

 
parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs


