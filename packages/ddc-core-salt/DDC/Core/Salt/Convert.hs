-- | Convert the Disciple Core Salt into to real C code.
--
--   The input module needs to be:
--      well typed,
--      fully named with no deBruijn indices,
--      have all functions defined at top-level,
--      a-normalised,
--      have a control-transfer primop at the end of every function body
--        (these are added by DDC.Core.Salt.Convert.Transfer)
--      
module DDC.Core.Salt.Convert
        ( Error (..)
        , seaOfSaltModule)
where
import DDC.Core.Salt.Convert.Prim
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Name
import DDC.Core.Salt.Platform
import DDC.Core.Collect
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Module                          as C
import DDC.Core.Exp
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Base.Pretty
import DDC.Control.Monad.Check                  (throw, result)
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map
import Control.Monad
import Data.Maybe


-- | Convert a Disciple Core Salt module to C-source text.
seaOfSaltModule
        :: Show a 
        => Bool                 -- ^ Whether to include top-level include macros.
        -> Platform             -- ^ Target platform specification
        -> Module a Name        -- ^ Module to convert.
        -> Either (Error a) Doc

seaOfSaltModule withPrelude pp mm
 = result $ convModuleM withPrelude pp mm


-- Module ---------------------------------------------------------------------
-- | Convert a Salt module to C source text.
convModuleM :: Show a => Bool -> Platform -> Module a Name -> ConvertM a Doc
convModuleM withPrelude pp mm@(ModuleCore{})
 | ([LRec bxs], _) <- splitXLets $ moduleBody mm
 = do   
        -- Top-level includes ---------
        let cIncludes
                | not withPrelude
                = []

                | otherwise
                = [ text "#include \"Runtime.h\""
                  , text "#include \"Primitive.h\""
                  , empty ]

        -- Import external symbols ----
        let nts = Map.elems $ C.moduleImportTypes mm
        docs    <- mapM (uncurry $ convFunctionType Env.empty) nts
        let cExterns
                |  not withPrelude
                = []

                | otherwise
                =  [ text "extern " <> doc <> semi | doc <- docs ]

        -- RTS def --------------------
        -- If this is the main module then we need to declare
        -- the global RTS state.
        let cGlobals
                | not withPrelude
                = []

                | isMainModule mm
                = [ text "addr_t _DDC_Runtime_heapTop = 0;"
                  , text "addr_t _DDC_Runtime_heapMax = 0;"
                  , empty ]

                | otherwise
                = [ text "extern addr_t _DDC_Runtime_heapTop;"
                  , text "extern addr_t _DDC_Runtime_heapMax;"
                  , empty ]

        -- Super-combinator definitions.
        let kenv = Env.fromTypeMap $ Map.map snd $ moduleImportKinds mm
        let tenv = Env.fromTypeMap $ Map.map snd $ moduleImportTypes mm
        cSupers <- mapM (uncurry (convSuperM pp kenv tenv)) bxs

        -- Paste everything together
        return  $  vcat 
                $  cIncludes 
                ++ cExterns
                ++ cGlobals 
                ++ cSupers

 | otherwise
 = throw $ ErrorNoTopLevelLetrec mm


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
convFunctionType 
        :: KindEnv  Name
        -> QualName Name        -- ^ Function name.
        -> Type     Name        -- ^ Function type.
        -> ConvertM a Doc

convFunctionType kenv nFunc tFunc
 | TForall b t' <- tFunc
 = convFunctionType (Env.extend b kenv) nFunc t'

 | otherwise
 = do   -- Qualifiers aren't supported yet.
        let QualName _ n = nFunc        
        let nFun'        = text $ sanitizeGlobal (renderPlain $ ppr n)

        let (tsArgs, tResult) = takeTFunArgResult tFunc

        tsArgs'          <- mapM (convTypeM kenv) tsArgs
        tResult'         <- convTypeM kenv tResult

        return $ tResult' <+> nFun' <+> parenss tsArgs'


-- Super definition -----------------------------------------------------------
-- | Convert a super to C source text.
convSuperM 
        :: Show a 
        => Platform
        -> KindEnv Name 
        -> TypeEnv Name
        -> Bind Name 
        -> Exp a Name 
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
        bsParam'        <- mapM (convBind kenv tenv) $ filter keepBind bsParam

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
                [ tResult'                        -- Function header.
                         <+> nTop'
                         <+> parenss bsParam'
                , lbrace
                ,       indent 8 $ vcat dsVal     -- Variable declarations.
                ,       empty
                ,       indent 8 (xBody' <> semi) -- Function body.
                ,       rbrace
                , empty]
        
 | otherwise    
 = throw $ ErrorFunctionInvalid xx


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
         
        BNone{} -> False         
        _       -> True


-- | Convert a function parameter binding to C source text.
convBind :: KindEnv Name -> TypeEnv Name -> Bind Name -> ConvertM a Doc
convBind kenv _tenv b
 = case b of 
   
        -- Named variables binders.
        BName (NameVar str) t
         -> do   t'      <- convTypeM kenv t
                 return  $ t' <+> (text $ sanitizeLocal str)
                 
        _       -> throw $ ErrorParameterInvalid b


-- Blocks ---------------------------------------------------------------------
-- | What context we're doing this conversion in.
data Context
        -- | Conversion at the top-level of a function.
        --   The expresison being converted must eventually pass control.
        = ContextTop

        -- | In a nested context, like in the right of a let-binding.
        --   The expression should produce a value that we assign to this
        --   variable.
        | ContextNest Name (Type Name)          
        deriving Show


-- | Convert an expression to a block of statements.
--
--   If this expression defines a top-level function then the block
--     must end with a control transfer primop like return# or tailcall#.
--    
--   The `Context` tells us what do do when we get to the end of the block.
--
convBlockM 
        :: Show a
        => Context
        -> Platform
        -> KindEnv Name
        -> TypeEnv Name
        -> Exp a Name
        -> ConvertM a Doc

convBlockM context pp kenv tenv xx
 = case xx of

        XApp{}
         -- If we're at the top-level of a function body then the 
         -- last statement must explicitly pass control.
         | ContextTop      <- context
         -> case takeXPrimApps xx of
                Just (NamePrimOp p, xs)
                 |  isControlPrim p || isCallPrim p
                 -> convPrimCallM pp kenv tenv p xs

                _ -> throw $ ErrorBodyMustPassControl xx

         -- If we're in a nested context but the primop we're 
         -- calling doesn't return, and doesn't return a value,
         -- then we can't assign it to the result var.
         | ContextNest{}         <- context
         , Just (NamePrimOp p, xs) <- takeXPrimApps xx
         , isControlPrim p || isCallPrim p
         -> convPrimCallM pp kenv tenv p xs

         -- In a nested context we assign the result value to the 
         -- provided variable.
         | ContextNest n _  <- context
         -> do  xx'     <- convRValueM pp kenv tenv xx
                let n'  = text $ sanitizeLocal (renderPlain $ ppr n)
                return  $ vcat 
                       [ fill 12 n' <+> equals <+> xx' <> semi ]

        -- ISSUE #251: Fix nested case expressions that assign to nothing binders
        --   Change the Name field of ContextNext to a Bind so we can use
        --   the BNone form.
        --
        -- Binding from a case-expression.
        XLet _ (LLet LetStrict b@(BName n t) x1@XCase{}) x2
         -> do  x1'     <- convBlockM (ContextNest n t) pp kenv tenv x1

                let tenv' = Env.extend b tenv 
                x2'     <- convBlockM context           pp kenv tenv' x2

                return  $ vcat
                        [ x1' <> semi
                        , x2' ]

        -- Variable assignment from an r-value.
        XLet _ (LLet LetStrict (BName (NameVar n) _) x1) x2
         -> do  x1'     <- convRValueM pp kenv tenv x1
                x2'     <- convBlockM  context pp kenv tenv x2
                let n'  = text $ sanitizeLocal n

                return  $ vcat
                        [ fill 12 n' <+> equals <+> x1' <> semi
                        , x2' ]

        -- Non-binding statement.
        --  We just drop any returned value on the floor.
        XLet _ (LLet LetStrict (BNone _) x1) x2
         -> do  x1'     <- convStmtM  context pp kenv tenv x1
                x2'     <- convBlockM context pp kenv tenv x2

                return  $ vcat
                        [ x1' <> semi
                        , x2' ]

        -- Ditch letregions.
        XLet _ (LLetRegions bs ws) x
         -> let kenv'   = Env.extends bs kenv
                tenv'   = Env.extends ws tenv
            in  convBlockM context pp kenv' tenv' x

        -- Case-expression.
        --   Prettier printing for case-expression that just checks for failure.
        XCase _ x [ AAlt (PData dc []) x1
                  , AAlt PDefault     xFail]
         | isFailX xFail
         , Just n       <- takeNameOfDaCon dc
         , Just n'      <- convDaConName n
         -> do  
                x'      <- convRValueM pp kenv tenv x
                x1'     <- convBlockM  context pp kenv tenv x1
                xFail'  <- convBlockM  context pp kenv tenv xFail

                return  $ vcat
                        [ text "if"
                                <+> parens (x' <+> text "!=" <+> n')
                                <+> xFail' <> semi
                        , x1' ]

        -- Case-expression.
        --   Prettier printing for if-then-else.
        XCase _ x [ AAlt (PData dc1 []) x1
                  , AAlt (PData dc2 []) x2 ]
         | Just (NameLitBool True)  <- takeNameOfDaCon dc1
         , Just (NameLitBool False) <- takeNameOfDaCon dc2
         -> do  x'      <- convRValueM pp kenv tenv x
                x1'     <- convBlockM context pp kenv tenv x1
                x2'     <- convBlockM context pp kenv tenv x2

                return  $ vcat
                        [ text "if" <> parens x'
                        , lbrace <> indent 7 x1' <> semi <> line <> rbrace
                        , text "else"
                        , lbrace <> indent 7 x2' <> semi <> line <> rbrace ]

        -- Case-expression.
        --   In the general case we use the C-switch statement.
        XCase _ x alts
         -> do  x'      <- convRValueM pp kenv tenv x
                alts'   <- mapM (convAltM context pp kenv tenv) alts

                return  $ vcat
                        [ text "switch" <+> parens x'
                        , lbrace <> indent 1 (vcat alts')
                        , rbrace ]

        -- Ditch casts.
        XCast _ _ x
         -> convBlockM context pp kenv tenv x

        _ -> throw $ ErrorBodyInvalid xx


-- | Check whether this primop passes control (and does not return).
isControlPrim :: PrimOp -> Bool
isControlPrim pp
 = case pp of
        PrimControl{}   -> True
        _               -> False


-- | Check whether this primop passes control (and returns).
isCallPrim :: PrimOp -> Bool
isCallPrim pp
 = case pp of
        PrimCall{}      -> True
        _               -> False


-- | Check whether this an application of the fail# primop.
isFailX  :: Exp a Name -> Bool
isFailX (XApp _ (XVar _ (UPrim (NamePrimOp (PrimControl PrimControlFail)) _)) _)
          = True
isFailX _ = False


-- Alt ------------------------------------------------------------------------
-- | Convert a case alternative to C source text.
convAltM 
        :: Show a 
        => Context
        -> Platform
        -> KindEnv Name
        -> TypeEnv Name
        -> Alt a Name 
        -> ConvertM a Doc

convAltM context pp kenv tenv aa
 = case aa of
        AAlt PDefault x1 
         -> do  x1'     <- convBlockM context pp kenv tenv x1
                return  $ vcat
                        [ text "default:" 
                        , lbrace <> indent 5 (x1' <> semi)
                                 <> line
                                 <> rbrace]

        AAlt (PData dc []) x1
         | Just n       <- takeNameOfDaCon dc
         , Just n'      <- convDaConName n
         -> do  x1'     <- convBlockM context pp kenv tenv x1
                return  $ vcat
                        [ text "case" <+> n' <> colon
                        , lbrace <> indent 5 (x1' <> semi)
                                 <> line
                                 <> rbrace]

        AAlt{} -> throw $ ErrorAltInvalid aa


-- | Convert a data constructor name to a pattern to use in a switch.
--
--   Only integral-ish types can be used as patterns, for others 
--   such as Floats we rely on the Lite transform to have expanded
--   cases on float literals into a sequence of boolean checks.
convDaConName :: Name -> Maybe Doc
convDaConName nn
 = case nn of
        NameLitBool True   -> Just $ int 1
        NameLitBool False  -> Just $ int 0

        NameLitNat  i      -> Just $ integer i

        NameLitInt  i      -> Just $ integer i

        NameLitWord i bits
         |  elem bits [8, 16, 32, 64]
         -> Just $ integer i

        NameLitTag i       -> Just $ integer i

        _                  -> Nothing


-- Stmt -----------------------------------------------------------------------
-- | Convert an effectful statement to C source text.
convStmtM 
        :: Show a 
        => Context
        -> Platform
        -> KindEnv Name
        -> TypeEnv Name 
        -> Exp a Name 
        -> ConvertM a Doc

convStmtM context pp kenv tenv xx
 = case xx of
        -- Primop application.
        XApp{}
          |  Just (NamePrimOp p, xs) <- takeXPrimApps xx
          -> convPrimCallM pp kenv tenv p xs

        -- Super application.
        XApp{}
         |  Just (XVar _ (UName n), args)  <- takeXApps xx
         ,  NameVar nTop <- n
         -> do  let nTop'       = sanitizeGlobal nTop

                args'           <- mapM (convRValueM pp kenv tenv)
                                $  filter keepFunArgX args

                return  $ text nTop' <+> parenss args'

        -- Ditch casts.
        XCast _ _ x
         -> convStmtM context pp kenv tenv x

        _ -> throw $ ErrorStmtInvalid xx


-- RValue ---------------------------------------------------------------------
-- | Convert an r-value to C source text.
convRValueM 
        :: Show a 
        => Platform
        -> KindEnv Name 
        -> TypeEnv Name 
        -> Exp a Name 
        -> ConvertM a Doc

convRValueM pp kenv tenv xx
 = case xx of

        -- Plain variable.
        XVar _ (UName n)
         | NameVar str  <- n
         -> return $ text $ sanitizeLocal str

        -- Literals
        XCon _ dc
         | DaConNamed n         <- daConName dc
         -> case n of
                NameLitNat  i   -> return $ integer i
                NameLitInt  i   -> return $ integer i
                NameLitWord i _ -> return $ integer i
                NameLitTag  i   -> return $ integer i
                NameLitVoid     -> return $ text "void"
                _               -> throw $ ErrorRValueInvalid xx

        -- Primop application.
        XApp{}
         |  Just (NamePrimOp p, args)      <- takeXPrimApps xx
         -> convPrimCallM pp kenv tenv p args

        -- Super application.
        XApp{}
         |  Just (XVar _ (UName n), args)  <- takeXApps xx
         ,  NameVar nTop <- n
         -> do  let nTop' = sanitizeGlobal nTop

                -- Ditch type and witness arguments
                args'   <- mapM (convRValueM pp kenv tenv) 
                        $  filter keepFunArgX args

                return  $ text nTop' <> parenss args'

        -- Type argument.
        XType t
         -> do  t'      <- convTypeM kenv t
                return  $ t'

        -- Ditch casts.
        XCast _ _ x
         -> convRValueM pp kenv tenv x

        _ -> throw $ ErrorRValueInvalid xx


-- | We don't need to pass types and witnesses to top-level supers.
keepFunArgX :: Exp a n -> Bool
keepFunArgX xx
 = case xx of
        XType{}         -> False
        XWitness{}      -> False
        _               -> True


-- PrimCalls ------------------------------------------------------------------
-- | Convert a call to a primitive operator to C source text.
convPrimCallM 
        :: Show a 
        => Platform
        -> KindEnv Name
        -> TypeEnv Name
        -> PrimOp
        -> [Exp a Name] -> ConvertM a Doc

convPrimCallM pp kenv tenv p xs
 = case p of

        -- Binary arithmetic primops.
        PrimArith op
         | [XType _t, x1, x2]   <- xs
         , Just op'             <- convPrimArith2 op
         -> do  x1'     <- convRValueM pp kenv tenv x1
                x2'     <- convRValueM pp kenv tenv x2
                return  $ parens (x1' <+> op' <+> x2')


        -- Cast primops.
        PrimCast PrimCastPromote
         | [XType tDst, XType tSrc, x1]    <- xs
         , Just (NamePrimTyCon tcSrc, _) <- takePrimTyConApps tSrc
         , Just (NamePrimTyCon tcDst, _) <- takePrimTyConApps tDst 
         , primCastPromoteIsValid pp tcSrc tcDst
         -> do  tDst'   <- convTypeM   kenv tDst
                x1'     <- convRValueM pp kenv tenv x1
                return  $  parens tDst' <> parens x1'

        PrimCast PrimCastTruncate
         | [XType tDst, XType tSrc, x1] <- xs
         , Just (NamePrimTyCon tcSrc, _) <- takePrimTyConApps tSrc
         , Just (NamePrimTyCon tcDst, _) <- takePrimTyConApps tDst 
         , primCastTruncateIsValid pp tcSrc tcDst
         -> do  tDst'   <- convTypeM   kenv tDst
                x1'     <- convRValueM pp kenv tenv x1
                return  $  parens tDst' <> parens x1'


        -- Control primops.
        PrimControl PrimControlReturn
         | [XType _t, x1]       <- xs
         -> do  x1'     <- convRValueM pp kenv tenv x1
                return  $ text "return" <+> x1'

        PrimControl PrimControlFail
         | [XType _t]           <- xs
         -> do  return  $ text "_FAIL()"


        -- Call primops.
        -- ISSUE #261: Implement tailcalls in the C backend.
        --   This doesn't actually do a tailcall.
        --   For straight tail-recursion we need to overwrite the parameters
        --   with the new arguments and jump back to the start of the function.
        PrimCall (PrimCallTail arity)
         | xFunTys : xsArgs     <- drop (arity + 1) xs
         , Just (xFun, _)       <- takeXApps xFunTys
         , XVar _ (UName n)     <- xFun
         , NameVar nTop         <- n
         -> do  let nFun'       = text $ sanitizeGlobal nTop
                xsArgs'         <- mapM (convRValueM pp kenv tenv) xsArgs
                return  $  text "return" <+> nFun' <> parenss xsArgs'


        -- Store primops.
        PrimStore op
         -> do  let op'  = convPrimStore op
                xs'     <- mapM (convRValueM pp kenv tenv) 
                        $  filter (keepArgX kenv) xs
                return  $ op' <> parenss xs'

        _ -> throw $ ErrorPrimCallInvalid p xs


-- | Throw away region arguments.
keepArgX :: KindEnv Name -> Exp a Name -> Bool
keepArgX kenv xx
 = case xx of
        XType (TVar u)
         |  Just k       <- Env.lookup u kenv
         -> isDataKind k 

        XWitness{}       -> False
        _                -> True


parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs

