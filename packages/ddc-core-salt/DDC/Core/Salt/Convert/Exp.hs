module DDC.Core.Salt.Convert.Exp
        ( Context(..)
        , convBlockM
        , convAltM
        , convRValueM
        , convPrimCallM)
where
import DDC.Core.Salt.Convert.Prim
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Convert.Type
import DDC.Core.Salt.Name
import DDC.Core.Salt.Platform
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Env                     (KindEnv, TypeEnv)
import DDC.Base.Pretty
import DDC.Control.Monad.Check          (throw)
import qualified DDC.Type.Env           as Env


-- Context ----------------------------------------------------------------------------------------
-- | What context we're doing this conversion in.
data Context
        -- | Conversion at the top-level of a function.
        --   The expresison being converted must eventually pass control.
        = ContextTop

        -- | In a nested context, like in the right of a let-binding.
        --   The expression should produce a value that we assign to this
        --   variable.
        | ContextNest (Bind Name)        
        deriving Show


-- | Check whether a context is nested.
isContextNest :: Context -> Bool
isContextNest cc
 = case cc of
        ContextNest{}   -> True
        _               -> False


-- Block ------------------------------------------------------------------------------------------
-- | Convert an expression to a block of statements.
--
--   If this expression defines a top-level function then the block
--     must end with a control transfer primop like return# or tailcall#.
--    
--   The `Context` tells us what do do when we get to the end of the block.
--
convBlockM 
        :: Show a
        => Context      -> Platform
        -> KindEnv Name -> TypeEnv Name
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
                 -> do  x1      <- convPrimCallM pp kenv tenv p xs
                        return  $ x1 <> semi

                _ -> throw $ ErrorBodyMustPassControl xx

         -- If we're in a nested context but the primop we're 
         -- calling doesn't return, and doesn't return a value,
         -- then we can't assign it to the result var.
         | ContextNest{}         <- context
         , Just (NamePrimOp p, xs) <- takeXPrimApps xx
         , isControlPrim p || isCallPrim p
         -> do  x1      <- convPrimCallM pp kenv tenv p xs
                return  $ x1 <> semi

        _ 
         -- In a nested context with a BName binder,
         --   assign the result value to the provided variable.
         | isRValue xx
         , ContextNest (BName n _)  <- context
         -> do  xx'     <- convRValueM pp kenv tenv xx
                let n'  = text $ sanitizeLocal (renderPlain $ ppr n)
                return  $ vcat 
                       [ fill 12 n' <+> equals <+> xx' <> semi ]

         -- In a nested context with a BNone binder,
         --   just drop the result on the floor.
         | isRValue xx
         , ContextNest  (BNone _)   <- context
         -> do  xx'     <- convRValueM pp kenv tenv xx
                return  $ vcat 
                       [ xx' <> semi ]

        -- Binding from a case-expression.
        XLet _ (LLet b x1@XCase{}) x2
         -> do  
                -- Convert the right hand side in a nested context.
                --  The ContextNext holds the var to assign the result to.
                x1'     <- convBlockM (ContextNest b) pp kenv tenv x1

                -- Convert the rest of the function.
                let tenv' = Env.extend b tenv 
                x2'     <- convBlockM context         pp kenv tenv' x2

                return  $ vcat
                        [ x1'
                        , x2' ]

        -- Binding from an r-value.
        XLet _ (LLet b x1) x2
         -> do  x1'     <- convRValueM pp kenv tenv x1
                x2'     <- convBlockM  context pp kenv tenv x2

                let dst = case b of
                           BName (NameVar n) _ 
                             -> fill 12 (text $ sanitizeLocal n) <+> equals <> space
                           _ -> empty

                return  $ vcat
                        [ dst <> x1' <> semi
                        , x2' ]

        -- Ditch letregions.
        XLet _ (LPrivate bs _mt ws) x
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
                                <+> xFail'
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
                        , lbrace <> indent 7 x1' <> line <> rbrace
                        , text "else"
                        , lbrace <> indent 7 x2' <> line <> rbrace ]

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


-- Alt --------------------------------------------------------------------------------------------
-- | Convert a case alternative to C source text.
convAltM 
        :: Show a 
        => Context      -> Platform
        -> KindEnv Name -> TypeEnv Name
        -> Alt a Name 
        -> ConvertM a Doc

convAltM context pp kenv tenv aa
 = let end 
        | isContextNest context = line <> text "break;"
        | otherwise             = empty
   
   in case aa of
        AAlt PDefault x1 
         -> do  x1'     <- convBlockM context pp kenv tenv x1
                return  $ vcat
                        [ text "default:" 
                        , lbrace <> indent 5 (x1' <> end)
                                 <> line
                                 <> rbrace]

        AAlt (PData dc []) x1
         | Just n       <- takeNameOfDaCon dc
         , Just n'      <- convDaConName n
         -> do  x1'     <- convBlockM context pp kenv tenv x1
                return  $ vcat
                        [ text "case" <+> n' <> colon
                        , lbrace <> indent 5 (x1' <> end)
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


-- RValue -----------------------------------------------------------------------------------------
-- | Convert an Right-value to C source text.
convRValueM 
        :: Show a 
        => Platform
        -> KindEnv Name -> TypeEnv Name 
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
         | DaConPrim n _        <- dc
         -> case n of
                NameLitBool b   
                 | b            -> return $ integer 1
                 | otherwise    -> return $ integer 0

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
        XType _ t
         -> do  t'      <- convTypeM kenv t
                return  $ t'

        -- Ditch casts.
        XCast _ _ x
         -> convRValueM pp kenv tenv x

        _ -> throw $ ErrorRValueInvalid xx


-- | Check if some expression is an r-value, 
--   meaning a variable, constructor, application or cast of one.
isRValue :: Exp a Name -> Bool
isRValue xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XApp{}          -> True
        XCast _ _ x     -> isRValue x
        _               -> False


-- | We don't need to pass types and witnesses to top-level supers.
keepFunArgX :: Exp a n -> Bool
keepFunArgX xx
 = case xx of
        XType{}         -> False
        XWitness{}      -> False
        _               -> True


-- PrimCalls --------------------------------------------------------------------------------------
-- | Convert a call to a primitive operator to C source text.
convPrimCallM 
        :: Show a 
        => Platform
        -> KindEnv Name -> TypeEnv Name
        -> PrimOp       -> [Exp a Name] 
        -> ConvertM a Doc

convPrimCallM pp kenv tenv p xs
 = case p of

        -- Binary arithmetic primops.
        PrimArith op
         | [XType _ _, x1, x2]  <- xs
         , Just op'             <- convPrimArith2 op
         -> do  x1'     <- convRValueM pp kenv tenv x1
                x2'     <- convRValueM pp kenv tenv x2
                return  $ parens (x1' <+> op' <+> x2')


        -- Cast primops.
        PrimCast PrimCastPromote
         | [XType _ tDst, XType _ tSrc, x1]    <- xs
         , Just (NamePrimTyCon tcSrc, _) <- takePrimTyConApps tSrc
         , Just (NamePrimTyCon tcDst, _) <- takePrimTyConApps tDst 
         , primCastPromoteIsValid pp tcSrc tcDst
         -> do  tDst'   <- convTypeM   kenv tDst
                x1'     <- convRValueM pp kenv tenv x1
                return  $  parens tDst' <> parens x1'

        PrimCast PrimCastTruncate
         | [XType _ tDst, XType _ tSrc, x1] <- xs
         , Just (NamePrimTyCon tcSrc, _) <- takePrimTyConApps tSrc
         , Just (NamePrimTyCon tcDst, _) <- takePrimTyConApps tDst 
         , primCastTruncateIsValid pp tcSrc tcDst
         -> do  tDst'   <- convTypeM   kenv tDst
                x1'     <- convRValueM pp kenv tenv x1
                return  $  parens tDst' <> parens x1'


        -- Control primops.
        PrimControl PrimControlReturn
         | [XType _ _, x1]       <- xs
         -> do  x1'     <- convRValueM pp kenv tenv x1
                return  $ text "return" <+> x1'

        PrimControl PrimControlFail
         | [XType _ _]           <- xs
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
                        $  filter (keepPrimArgX kenv) xs
                return  $ op' <> parenss xs'

        _ -> throw $ ErrorPrimCallInvalid p xs


-- | Ditch region arguments.
keepPrimArgX :: KindEnv Name -> Exp a Name -> Bool
keepPrimArgX kenv xx
 = case xx of
        XType _ (TVar u)
         |  Just k       <- Env.lookup u kenv
         -> isDataKind k 

        XWitness{}       -> False
        _                -> True


parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs

