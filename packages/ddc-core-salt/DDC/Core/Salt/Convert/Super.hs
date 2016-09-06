module DDC.Core.Salt.Convert.Super
        (convSuperM)
where
import DDC.Core.Salt.Convert.Exp
import DDC.Core.Salt.Convert.Type
import DDC.Core.Salt.Convert.Name
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Name
import DDC.Core.Salt.Platform
import DDC.Core.Collect
import DDC.Core.Module
import DDC.Core.Exp.Annot
import DDC.Type.Env                     (KindEnv, TypeEnv)
import DDC.Data.Pretty
import DDC.Control.Monad.Check          (throw)
import qualified DDC.Type.Env           as Env
import Control.Monad
import Data.Maybe


-- | Convert a supercombinator definition to C source text.
convSuperM 
        :: Show a 
        => Platform                     -- ^ Target platform specifiction.
        -> Module a Name                -- ^ Enclosing module.
        -> KindEnv Name                 -- ^ Top-level kind environment.
        -> TypeEnv Name                 -- ^ Top-level type environment.
        -> Name                         -- ^ Name of the supercombinator.
        -> Type Name                    -- ^ Type of the supercombinator.
        -> Exp a Name                   -- ^ Body expression of the supercombiantor.
        -> ConvertM a Doc

convSuperM     pp mm kenv0 tenv0 nSuper tSuper xx
 = convSuperM' pp mm kenv0 tenv0 nSuper tSuper [] xx

convSuperM'    pp mm kenv  tenv  nSuper tSuper bsParam xx
 
 -- Enter into type abstractions,
 --  adding the bound name to the environment.
 | XLAM _ b x   <- xx
 = convSuperM' pp mm (Env.extend b kenv) tenv 
        nSuper tSuper bsParam x

 -- Enter into value abstractions,
 --  remembering that we're now in a function that has this parameter.
 | XLam _ b x   <- xx
 = convSuperM' pp mm kenv (Env.extend b tenv) 
        nSuper tSuper (bsParam ++ [b]) x

 -- Convert the function body.
 | otherwise
 = do   
        -- Convert the function name.
        let Just nSuper' = seaNameOfSuper 
                                (lookup nSuper (moduleImportValues mm))
                                (lookup nSuper (moduleExportValues mm))
                                nSuper
        
        let (_, tResult) = takeTFunArgResult $ eraseTForalls tSuper

        -- Convert the function parameters.
        bsParam'        <- sequence
                        $ [ convBind kenv tenv i b
                                | b     <- filter keepBind bsParam
                                | i     <- [0..] ]

        -- Convert the result type.
        tResult'        <- convTypeM kenv  $ eraseWitArg tResult

        -- Emit automatic variable definitions for all the local variables
        -- used in the body of the function.
        let (_, bsVal)  = collectBinds xx
        dsVal           <- liftM catMaybes $ mapM (makeVarDecl kenv) 
                        $  filter keepBind bsVal

        -- Convert the body of the function.
        --   We pass in ContextTop to say we're at the top-level of the function,
        ---  so the block must explicitly pass control in the final statement.
        let config      = Config
                        { configPlatform = pp
                        , configModule   = mm }
        
        xBody'          <- convBlockM config ContextTop kenv tenv xx

        -- Paste everything together.
        return  $ vcat
                [ -- Function header.
                  tResult'                      -- Result type.
                         <+> nSuper'            -- Function name.
                         <+> parenss bsParam'   -- Argument list.
                  
                  -- Function body.
                , lbrace
                ,       indent 8 $ vcat dsVal   -- Local variable definitions.
                ,       empty
                ,       indent 8 xBody'         -- Statements that do some things.
                , rbrace
                , empty]
        

-- | Convert a function parameter binding to C source text.
convBind :: KindEnv Name -> TypeEnv Name -> Int -> Bind Name -> ConvertM a Doc
convBind kenv _tenv iPos b
 = case b of 
        -- Anonymous arguments.
        BNone t
         -> do  t'      <- convTypeM kenv t
                return  $ t' <+> (text $ "_arg" ++ show iPos)

        -- Named variables binders.
        BName n t
         | Just n'      <- seaNameOfLocal n
         -> do  t'      <- convTypeM kenv t
                return  $ t' <+> n'

        _       -> throw $ ErrorParameterInvalid b


-- | Make a variable declaration for this binder.
makeVarDecl :: KindEnv Name -> Bind Name -> ConvertM a (Maybe Doc)
makeVarDecl kenv bb
 = case bb of
        BNone{} 
         -> return Nothing

        BName n t
         | Just n'      <- seaNameOfLocal n
         -> do  t'      <- convTypeM kenv t
                return  $ Just (t' <+> n' <+> equals <+> text "0" <> semi)

        _       -> throw $ ErrorParameterInvalid bb


-- | Remove witness arguments from the return type
eraseWitArg :: Type Name -> Type Name
eraseWitArg tt
 = case tt of 
        -- Distinguish between application of witnesses and ptr
        TApp _ t2
         | Just (NamePrimTyCon PrimTyConPtr, _) <- takePrimTyConApps tt -> tt
         | otherwise -> eraseWitArg t2

        -- Pass through all other types
        _ -> tt


-- | Ditch witness bindings
keepBind :: Bind Name -> Bool
keepBind bb
 = case bb of        
        BName _ t
         |  tc : _ <- takeTApps t
         ,  isWitnessType tc
         -> False
         
        _       -> True


parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs

