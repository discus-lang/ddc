module DDC.Core.Salt.Convert.Super
        (convSuperM)
where
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Convert.Type
import DDC.Core.Salt.Convert.Exp
import DDC.Core.Salt.Name
import DDC.Core.Salt.Platform
import DDC.Core.Collect
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Env                     (KindEnv, TypeEnv)
import DDC.Base.Pretty
import DDC.Control.Monad.Check          (throw)
import qualified DDC.Type.Env           as Env
import Control.Monad
import Data.Maybe


-- | Convert a supercombinator definition to C source text.
convSuperM 
        :: Show a 
        => Platform             -- ^ Target platform specifiction.
        -> KindEnv Name         -- ^ Top-level kind environment.
        -> TypeEnv Name         -- ^ Top-level type environment.
        -> Bind Name            -- ^ Binder for the supercombinator.
        -> Exp a Name           -- ^ Body expression of the supercombiantor.
        -> ConvertM a Doc

convSuperM     pp kenv0 tenv0 bTop xx
 = convSuperM' pp kenv0 tenv0 bTop [] xx

convSuperM' pp kenv tenv bTop bsParam xx
 
 -- Enter into type abstractions,
 --  adding the bound name to the environment.
 | XLAM _ b x   <- xx
 = convSuperM' pp (Env.extend b kenv) tenv bTop bsParam x

 -- Enter into value abstractions,
 --  remembering that we're now in a function that has this parameter.
 | XLam _ b x   <- xx
 = convSuperM' pp kenv (Env.extend b tenv) bTop (bsParam ++ [b]) x

 -- Convert the function body.
 | BName (NameVar nTop) tTop <- bTop
 = do   

        -- Convert the function name.
        let nTop'        = text $ sanitizeGlobal nTop
        let (_, tResult) = takeTFunArgResult $ eraseTForalls tTop 

        -- Convert function parameters.
        bsParam'        <- sequence
                        $ [ convBind kenv tenv i b
                                | b     <- filter keepBind bsParam
                                | i     <- [0..] ]
                                        
        -- Convert result type.
        tResult'        <- convTypeM kenv  $ eraseWitArg tResult

        -- Emit variable definitions for all the value binders in the code.
        let (_, bsVal)  = collectBinds xx
        dsVal           <- liftM catMaybes $ mapM (makeVarDecl kenv) 
                        $  filter keepBind bsVal

        -- Convert the body of the function.
        --  We pass in ContextTop to say we're at the top-level
        --  of the function, so the block must explicitly pass
        --  control in the final statement.
        xBody'          <- convBlockM ContextTop pp kenv tenv xx

        return  $ vcat
                [ -- Function header.
                  tResult'                        
                         <+> nTop'
                         <+> parenss bsParam'
                , lbrace
                        -- Variable declarations.
                ,       indent 8 $ vcat dsVal     
                ,       empty

                         -- Function body.
                ,       indent 8 xBody'
                ,       rbrace
                , empty]
        
 | otherwise    
 = throw $ ErrorFunctionInvalid xx


-- | Convert a function parameter binding to C source text.
convBind :: KindEnv Name -> TypeEnv Name -> Int -> Bind Name -> ConvertM a Doc
convBind kenv _tenv iPos b
 = case b of 
        -- Named variables binders.
        BName (NameVar str) t
         -> do  t'      <- convTypeM kenv t
                return  $ t' <+> (text $ sanitizeLocal str)
        
        -- Anonymous arguments.
        BNone t
         -> do  t'      <- convTypeM kenv t
                return  $ t' <+> (text $ "_arg" ++ show iPos)

        _       -> throw $ ErrorParameterInvalid b


-- | Make a variable declaration for this binder.
makeVarDecl :: KindEnv Name -> Bind Name -> ConvertM a (Maybe Doc)
makeVarDecl kenv bb
 = case bb of
        BNone{} 
         -> return Nothing

        BName (NameVar n) t
         -> do  t'      <- convTypeM kenv t
                let n'  = text $ sanitizeLocal n
                return  $ Just (t' <+> n' <+> equals <+> text "0" <> semi)

        _ -> throw $ ErrorParameterInvalid bb


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

