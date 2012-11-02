
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
        , convertModule)
where
import DDC.Core.Salt.Convert.Prim
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Name
import DDC.Core.Collect
import DDC.Core.Compounds
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Env                     (KindEnv, TypeEnv)
import DDC.Core.Module                  as C
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Type.Check.Monad             (throw, result)
import qualified DDC.Type.Env           as Env
import qualified Data.Map               as Map
import Control.Monad
import Data.Maybe


-- | Convert a Disciple Core Salt module to C-source text.
convertModule 
        :: Show a 
        => Bool                 -- ^ Whether to include top-level include macros.
        -> Module a Name        -- ^ Module to convert.
        -> Either (Error a) Doc

convertModule withPrelude mm
 = result $ convModuleM withPrelude mm


-- Module ---------------------------------------------------------------------
-- | Convert a Salt module to C source text.
convModuleM :: Show a => Bool -> Module a Name -> ConvertM a Doc
convModuleM withPrelude mm@(ModuleCore{})
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
        cSupers <- mapM (uncurry (convSuperM kenv tenv)) bxs

        -- Pase everything together
        return  $  vcat 
                $  cIncludes 
                ++ cExterns
                ++ cGlobals 
                ++ cSupers

 | otherwise
 = throw $ ErrorNoTopLevelLetrec mm


-- Type -----------------------------------------------------------------------
-- | Convert a Salt type to C source text.
--   This only handles non-function types.
convTypeM :: KindEnv Name -> Type Name -> ConvertM a Doc
convTypeM kenv tt
 = case tt of
        TVar u
         -> case Env.lookup u kenv of
             Nothing            
              -> error $ "convertTypeM Type variable not in kind environment." ++ show u

             Just k
              | isDataKind k -> return $ text "Obj*"
              | otherwise    
              -> error $  "convertTypeM: Invalid type variable." 
                       ++ show (u, Env.envMap kenv)

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
 = do   -- TODO: print the qualifier when we start using them.
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
        => KindEnv Name 
        -> TypeEnv Name
        -> Bind Name 
        -> Exp a Name 
        -> ConvertM a Doc

convSuperM     kenv0 tenv0 bTop xx
 = convSuperM' kenv0 tenv0 bTop [] xx

convSuperM' kenv tenv bTop bsParam xx
 -- Enter into type abstractions,
 --  adding the bound name to the environment.
 | XLAM _ b x   <- xx
 = convSuperM' (Env.extend b kenv) tenv bTop bsParam x

 -- Enter into value abstractions,
 --  remembering that we're now in a function that has this parameter.
 | XLam _ b x   <- xx
 = convSuperM' kenv (Env.extend b tenv) bTop (bsParam ++ [b]) x

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
        xBody'          <- convBlockM ContextTop kenv tenv xx

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
        | ContextNest Name (Type Name)          -- TODO change this to a BIND 
                                                -- to handle nothing binders.
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
        -> KindEnv Name
        -> TypeEnv Name
        -> Exp a Name
        -> ConvertM a Doc

convBlockM context kenv tenv xx
 = case xx of

        XApp{}
         -- If we're at the top-level of a function body then the 
         -- last statement must explicitly pass control.
         | ContextTop      <- context
         -> case takeXPrimApps xx of
                Just (NamePrim p, xs)
                 |  isControlPrim p || isCallPrim p
                 -> convPrimCallM kenv tenv p xs

                _ -> throw $ ErrorBodyMustPassControl xx

         -- If we're in a nested context but the primop we're 
         -- calling doesn't return, and doesn't return a value,
         -- then we can't assign it to the result var.
         | ContextNest{}         <- context
         , Just (NamePrim p, xs) <- takeXPrimApps xx
         , isControlPrim p || isCallPrim p
         -> convPrimCallM kenv tenv p xs

         -- In a nested context we assign the result value to the 
         -- provided variable.
         | ContextNest n _  <- context
         -> do  xx'     <- convRValueM kenv tenv xx
                let n'  = text $ sanitizeLocal (renderPlain $ ppr n)
                return  $ vcat 
                       [ fill 12 n' <+> equals <+> xx' <> semi ]

        -- Binding from a case-expression.
        -- TODO: handle assignment to none binder.
        XLet _ (LLet LetStrict b@(BName n t) x1@XCase{}) x2
         -> do  x1'     <- convBlockM (ContextNest n t) kenv tenv x1

                let tenv' = Env.extend b tenv 
                x2'     <- convBlockM context         kenv tenv' x2

                return  $ vcat
                        [ x1' <> semi
                        , x2' ]

        -- Variable assignment from an r-value.
        XLet _ (LLet LetStrict (BName (NameVar n) _) x1) x2
         -> do  x1'     <- convRValueM kenv tenv x1
                x2'     <- convBlockM  context kenv tenv x2
                let n'  = text $ sanitizeLocal n

                return  $ vcat
                        [ fill 12 n' <+> equals <+> x1' <> semi
                        , x2' ]

        -- Non-binding statement.
        --  We just drop any returned value on the floor.
        XLet _ (LLet LetStrict (BNone _) x1) x2
         -> do  x1'     <- convStmtM  context kenv tenv x1
                x2'     <- convBlockM context kenv tenv x2

                return  $ vcat
                        [ x1' <> semi
                        , x2' ]

        -- Ditch letregions.
        XLet _ (LLetRegions bs ws) x
         -> let kenv'   = Env.extends bs kenv
                tenv'   = Env.extends ws tenv
            in  convBlockM context kenv' tenv' x

        -- Case-expression.
        --   Prettier printing for case-expression that just checks for failure.
        XCase _ x [ AAlt (PData dc []) x1
                  , AAlt PDefault     xFail]
         | isFailX xFail
         , Just n       <- takeNameOfDaCon dc
         , Just n'      <- convDaConName n
         -> do  
                x'      <- convRValueM kenv tenv x
                x1'     <- convBlockM  context kenv tenv x1
                xFail'  <- convBlockM  context kenv tenv xFail

                return  $ vcat
                        [ text "if"
                                <+> parens (x' <+> text "!=" <+> n')
                                <+> xFail' <> semi
                        , x1' ]

        -- Case-expression.
        --   Prettier printing for if-then-else.
        XCase _ x [ AAlt (PData dc1 []) x1
                  , AAlt (PData dc2 []) x2 ]
         | Just (NameBool True)  <- takeNameOfDaCon dc1
         , Just (NameBool False) <- takeNameOfDaCon dc2
         -> do  x'      <- convRValueM kenv tenv x
                x1'     <- convBlockM context kenv tenv x1
                x2'     <- convBlockM context kenv tenv x2

                return  $ vcat
                        [ text "if" <> parens x'
                        , lbrace <> indent 7 x1' <> semi <> line <> rbrace
                        , text "else"
                        , lbrace <> indent 7 x2' <> semi <> line <> rbrace ]

        -- Case-expression.
        --   In the general case we use the C-switch statement.
        XCase _ x alts
         -> do  x'      <- convRValueM kenv tenv x
                alts'   <- mapM (convAltM context kenv tenv) alts

                return  $ vcat
                        [ text "switch" <+> parens x'
                        , lbrace <> indent 1 (vcat alts')
                        , rbrace ]

        -- Ditch casts.
        XCast _ _ x
         -> convBlockM context kenv tenv x

        _ -> throw $ ErrorBodyInvalid xx


-- | Check whether this primop passes control (and does not return).
isControlPrim :: Prim -> Bool
isControlPrim pp
 = case pp of
        PrimControl{}   -> True
        _               -> False


-- | Check whether this primop passes control (and returns).
isCallPrim :: Prim -> Bool
isCallPrim pp
 = case pp of
        PrimCall{}      -> True
        _               -> False


-- | Check whether this an application of the fail# primop.
isFailX  :: Exp a Name -> Bool
isFailX (XApp _ (XVar _ (UPrim (NamePrim (PrimControl PrimControlFail)) _)) _) = True
isFailX _ = False


-- Alt ------------------------------------------------------------------------
-- | Convert a case alternative to C source text.
convAltM 
        :: Show a 
        => Context
        -> KindEnv Name
        -> TypeEnv Name
        -> Alt a Name 
        -> ConvertM a Doc

convAltM context kenv tenv aa
 = case aa of
        AAlt PDefault x1 
         -> do  x1'     <- convBlockM context kenv tenv x1
                return  $ vcat
                        [ text "default:" 
                        , lbrace <> indent 5 (x1' <> semi)
                                 <> line
                                 <> rbrace]

        AAlt (PData dc []) x1
         | Just n       <- takeNameOfDaCon dc
         , Just n'      <- convDaConName n
         -> do  x1'     <- convBlockM context kenv tenv x1
                return  $ vcat
                        [ text "case" <+> n' <> colon
                        , lbrace <> indent 5 (x1' <> semi)
                                 <> line
                                 <> rbrace]

        AAlt{} -> throw $ ErrorAltInvalid aa


convDaConName :: Name -> Maybe Doc
convDaConName nn
 = case nn of
        NameBool True   -> Just $ int 1
        NameBool False  -> Just $ int 0

        NameNat  i      -> Just $ integer i

        NameInt  i      -> Just $ integer i

        NameWord i bits
         |  elem bits [8, 16, 32, 64]
         -> Just $ integer i

        NameTag i       -> Just $ integer i

        _               -> Nothing


-- Stmt -----------------------------------------------------------------------
-- | Convert an effectful statement to C source text.
convStmtM 
        :: Show a 
        => Context
        -> KindEnv Name
        -> TypeEnv Name 
        -> Exp a Name 
        -> ConvertM a Doc

convStmtM context kenv tenv xx
 = case xx of
        -- Primop application.
        XApp{}
          |  Just (NamePrim p, xs) <- takeXPrimApps xx
          -> convPrimCallM kenv tenv p xs

        -- Super application.
        XApp{}
         |  Just (XVar _ (UName n), args)  <- takeXApps xx
         ,  NameVar nTop <- n
         -> do  let nTop'       = sanitizeGlobal nTop

                args'           <- mapM (convRValueM kenv tenv)
                                $  filter keepFunArgX args

                return  $ text nTop' <+> parenss args'

        -- Ditch casts.
        XCast _ _ x
         -> convStmtM context kenv tenv x

        _ -> throw $ ErrorStmtInvalid xx


-- RValue ---------------------------------------------------------------------
-- | Convert an r-value to C source text.
convRValueM 
        :: Show a 
        => KindEnv Name 
        -> TypeEnv Name 
        -> Exp a Name 
        -> ConvertM a Doc

convRValueM kenv tenv xx
 = case xx of

        -- Plain variable.
        XVar _ (UName n)
         | NameVar str  <- n
         -> return $ text $ sanitizeLocal str

        -- Literals
        XCon _ dc
         | DaConNamed n         <- daConName dc
         -> case n of
                NameNat  i      -> return $ integer i
                NameInt  i      -> return $ integer i
                NameWord i _    -> return $ integer i
                NameTag  i      -> return $ integer i
                NameVoid        -> return $ text "void"
                _               -> throw $ ErrorRValueInvalid xx

        -- Primop application.
        XApp{}
         |  Just (NamePrim p, args)        <- takeXPrimApps xx
         -> convPrimCallM kenv tenv p args

        -- Super application.
        XApp{}
         |  Just (XVar _ (UName n), args)  <- takeXApps xx
         ,  NameVar nTop <- n
         -> do  let nTop' = sanitizeGlobal nTop

                -- Ditch type and witness arguments
                args'   <- mapM (convRValueM kenv tenv) 
                        $  filter keepFunArgX args

                return  $ text nTop' <> parenss args'

        -- Type argument.
        XType t
         -> do  t'      <- convTypeM kenv t
                return  $ t'

        -- Ditch casts.
        XCast _ _ x
         -> convRValueM kenv tenv x

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
        => KindEnv Name
        -> TypeEnv Name
        -> Prim 
        -> [Exp a Name] -> ConvertM a Doc

convPrimCallM kenv tenv p xs
 = case p of

        -- Binary arithmetic primops.
        PrimOp op
         | [XType _t, x1, x2]   <- xs
         , Just op'             <- convPrimOp2 op
         -> do  x1'     <- convRValueM kenv tenv x1
                x2'     <- convRValueM kenv tenv x2
                return  $ parens (x1' <+> op' <+> x2')


        -- Cast primops.
        -- TODO: check for valid promotion
        PrimCast PrimCastPromote
         | [XType tTo, XType _tFrom, x1] <- xs
         -> do  tTo'    <- convTypeM   kenv tTo
                x1'     <- convRValueM kenv tenv x1
                return  $  parens tTo' <> parens x1'

        -- TODO: check for valid truncate
        PrimCast PrimCastTruncate
         | [XType tTo, XType _tFrom, x1] <- xs
         -> do  tTo'    <- convTypeM   kenv tTo
                x1'     <- convRValueM kenv tenv x1
                return  $  parens tTo' <> parens x1'


        -- Control primops.
        PrimControl PrimControlReturn
         | [XType _t, x1]       <- xs
         -> do  x1'     <- convRValueM kenv tenv x1
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
                xsArgs'         <- mapM (convRValueM kenv tenv) xsArgs
                return  $  nFun' <> parenss xsArgs'


        -- Store primops.
        PrimStore op
         -> do  let op'  = convPrimStore op
                xs'     <- mapM (convRValueM kenv tenv) 
                        $  filter (keepArgX kenv) xs
                return  $ op' <> parenss xs'


        -- External primops.
        PrimExternal op 
         -> do  let op' = convPrimExternal op
                xs'     <- mapM (convRValueM kenv tenv) 
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

