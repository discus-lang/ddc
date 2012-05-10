
-- | Convert the Disciple Core Salt into to real C code.
module DDC.Core.Salt.Convert
        (convertModule)
where
import DDC.Core.Salt.Error
import DDC.Core.Salt.Name
import DDC.Core.Salt.Sanitize
import DDC.Core.Compounds
import DDC.Type.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Type.Check.Monad             (throw, result)
import qualified DDC.Type.Check.Monad   as G


-- | Convert a Disciple Core Brine module to C-source text.
convertModule :: Show a => Module a Name -> Either (Error a) Doc
convertModule mm
 = result $ convModuleM mm


-- | Conversion Monad
type ConvertM a x = G.CheckM (Error a) x


-- Module ---------------------------------------------------------------------
-- | Convert a Brine module to C source text.
convModuleM :: Show a => Module a Name -> ConvertM a Doc
convModuleM mm@(ModuleCore{})
        | ([LRec bxs], _) <- splitXLets $ moduleBody mm
        = do    supers' <- mapM (uncurry convSuperM) bxs
                return  $ vcat supers'

        | otherwise
        = throw $ ErrorNoTopLevelLetrec mm


-- Type -----------------------------------------------------------------------
-- | Convert a Salt type to C source text.
convTypeM :: Type Name -> ConvertM a Doc
convTypeM tt
  = case tt of
        TCon (TyConBound (UPrim (NamePrimTyCon tc) _))
         |  Just doc     <- convPrimTyCon tc
         -> return doc

        TApp (TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConPtr) _))) t2
         -> do  t2'     <- convTypeM t2
                return  $ t2' <> text "*"

        TCon (TyConBound (UPrim NameObjTyCon _))
           ->   return  $ text "Obj"

        _  -> return $ text "DUNNO" -- throw $ ErrorTypeInvalid tt


-- | Convert a primitive type constructor to C source text.
convPrimTyCon :: PrimTyCon -> Maybe Doc
convPrimTyCon tc
 = case tc of
        PrimTyConVoid           -> Just $ text "void"
        PrimTyConAddr           -> Just $ text "addr_t"
        PrimTyConNat            -> Just $ text "nat_t"
        PrimTyConTag            -> Just $ text "tag_t"
        PrimTyConBool           -> Just $ text "bool_t"
        PrimTyConInt  bits      -> Just $ text "int"  <> int bits <> text "_t"
        PrimTyConWord bits      -> Just $ text "uint" <> int bits <> text "_t"
        PrimTyConString         -> Just $ text "string_t"
        _                       -> Nothing


-- Super definition -----------------------------------------------------------
-- | Convert a super to C source text.
convSuperM :: Show a => Bind Name -> Exp a Name -> ConvertM a Doc
convSuperM b x
 | BName (NameVar nTop) tTop       <- b 
 , Just (bsParam, xBody) <- takeXLams x
 ,  (_, tResult)         <- takeTFunArgResult tTop
 = do    
        let nTop'       =  text $ sanitizeName nTop
        bsParam'        <- mapM convBind bsParam
        tResult'        <- convTypeM tResult
        xBody'          <- convBodyM xBody

        return  $ vcat
                [ tResult'
                         <+> nTop'
                         <+> parenss bsParam'
                , lbrace <> line
                         <> indent 8 (xBody' <> semi) <> line
                         <> rbrace
                         <> line ]

 | otherwise    
 = throw $ ErrorFunctionInvalid x


-- | Convert a function parameter binding to C source text.
convBind :: Bind Name -> ConvertM a Doc
convBind (BName (NameVar str) t)
 = do   t'      <- convTypeM t
        return  $ t' <+> (text $ sanitizeName str)

convBind b 
 = throw $ ErrorParameterInvalid b


-- Super body -----------------------------------------------------------------
-- | Convert a super body to C source text.
convBodyM :: Show a => Exp a Name -> ConvertM a Doc
convBodyM xx
 = case xx of

        -- End of function body must explicitly pass control.
        XApp{}
         -> case takeXPrimApps xx of
             Just (NamePrim p, xs)
              | isControlPrim p -> convPrimCallM p xs
             _                  -> throw $ ErrorBodyMustPassControl xx

        -- Variable assignment.
        XLet _ (LLet LetStrict (BName (NameVar n) t) x1) x2
         -> do  t'      <- convTypeM   t
                x1'     <- convRValueM x1
                x2'     <- convBodyM   x2
                let n'  = text $ sanitizeName n

                return  $ vcat
                        [ fill 16 (t' <+> n') <+> equals <+> x1' <> semi
                        , x2' ]

        -- Non-binding statement.
        -- These are only permitted to return Void#.
        XLet _ (LLet LetStrict (BNone t) x1) x2
         | isVoidT t
         -> do  x1'     <- convStmtM x1
                x2'     <- convBodyM x2

                return  $ vcat
                        [ x1' <> semi
                        , x2' ]

         |  otherwise
         -> throw $ ErrorStmtNoDiscard xx

        -- Case-expression.
        --   Prettier printing for case-expression that just checks for failure.
        XCase _ x [ AAlt (PData (UPrim n _) []) x1
                  , AAlt PDefault     xFail]
         | isFailX xFail
         , Just n'      <- convDaConName n
         -> do  
                x'      <- convRValueM x
                x1'     <- convBodyM   x1
                xFail'  <- convBodyM   xFail

                return  $ vcat
                        [ text "if"
                                <+> parens (x' <+> text "!=" <+> n')
                                <+> xFail' <> semi
                        , x1' ]

        -- Case-expression.
        --   Prettier printing for if-then-else.
        XCase _ x [ AAlt (PData (UPrim (NameBool True)  _) []) x1
                  , AAlt (PData (UPrim (NameBool False) _) []) x2 ]
         -> do  x'      <- convRValueM x
                x1'     <- convBodyM   x1
                x2'     <- convBodyM   x2

                return  $ vcat
                        [ text "if" <> parens x'
                        , lbrace <> indent 7 x1' <> semi <> line <> rbrace
                        , lbrace <> indent 7 x2' <> semi <> line <> rbrace ]

        -- Case-expression.
        --   In the general case we use the C-switch statement.
        XCase _ x alts
         -> do  x'      <- convRValueM x
                alts'   <- mapM convAltM alts

                return  $ vcat
                        [ text "switch" <+> parens x'
                        , lbrace <> indent 1 (vcat alts')
                        , rbrace ]

        _ -> throw $ ErrorBodyInvalid xx

-- | Check whether this primop passes control.
isControlPrim :: Prim -> Bool
isControlPrim pp
 = case pp of
        PrimControl _   -> True
        _               -> False


-- | Check whether this is the Void# type.
isVoidT :: Type Name -> Bool
isVoidT (TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid) _))) = True
isVoidT _ = False


-- | Check whether this an applicatin of the fail# primop.
isFailX  :: Exp a Name -> Bool
isFailX (XApp _ (XVar _ (UPrim (NamePrim (PrimControl PrimControlFail)) _)) _) = True
isFailX _ = False


-- Stmt -----------------------------------------------------------------------
-- | Convert an effectful statement to C source text.
convStmtM :: Show a => Exp a Name -> ConvertM a Doc
convStmtM xx
 = case xx of
        XApp{}
          |  Just (NamePrim p, xs) <- takeXPrimApps xx
          -> convPrimCallM p xs

        _ -> throw $ ErrorStmtInvalid xx


-- Alt ------------------------------------------------------------------------
-- | Convert a case alternative to C source text.
convAltM :: Show a => Alt a Name -> ConvertM a Doc
convAltM aa
 = case aa of
        AAlt PDefault x1 
         -> do  x1'     <- convBodyM x1
                return  $ text "default:" <+> x1' <> semi


        AAlt (PData (UPrim n _) []) x1
         | Just n'      <- convDaConName n
         -> do  x1'     <- convBodyM x1
                return  $ vcat
                        [ text "case" <+> n' <> colon
                        , lbrace <> indent 5 (x1' <> semi)
                                 <> line
                                 <> rbrace]

        AAlt{} -> throw $ ErrorAltInvalid aa


convDaConName :: Name -> Maybe Doc
convDaConName nn
 = case nn of
        NameNat i       -> Just $ integer i
        NameTag i       -> Just $ integer i
        NameBool True   -> Just $ int 1
        NameBool False  -> Just $ int 0
        _               -> Nothing


-- RValue ---------------------------------------------------------------------
-- | Convert an r-value to C source text.
convRValueM :: Show a => Exp a Name -> ConvertM a Doc
convRValueM xx
 = case xx of

        -- Plain variable.
        XVar _ (UName n _)
         | NameVar str  <- n
         -> return $ text $ sanitizeName str

        -- Literals
        XCon _ (UPrim (NameNat n) _)
         -> return $ integer n

        XCon _ (UPrim (NameTag n) _)    
         -> return $ integer n

        XCon _ (UPrim (NameInt n _) _)    
         -> return $ integer n

        XCon _ (UPrim (NameWord n _) _)    
         -> return $ integer n

        -- Primop application.
        XApp{}
         |  Just (NamePrim p, args)      <- takeXPrimApps xx
         -> convPrimCallM p args

        -- Super application.
        XApp{}
         |  (XVar _ (UName n _) : args)  <- takeXApps xx
         ,  NameVar nTop <- n
         -> do  let nTop' = sanitizeName nTop
                args'     <- mapM convRValueM args
                return  $ text nTop' <+> parenss args'

        -- Type argument.
        XType t
         -> do  t'      <- convTypeM t
                return  $ t'

        _ -> error (show xx) -- throw $ ErrorRValueInvalid xx



-- PrimCalls ------------------------------------------------------------------
-- | Convert a call to a primitive operator to C source text.
convPrimCallM :: Show a => Prim -> [Exp a Name] -> ConvertM a Doc
convPrimCallM p xs
 = case p of

        -- Binary arithmetic primops.
        PrimOp op
         | [XType _t, x1, x2]   <- xs
         , Just op'             <- convPrimOp2 op
         -> do  x1'     <- convRValueM x1
                x2'     <- convRValueM x2
                return  $ parens (x1' <+> op' <+> x2')


        -- Cast primops.
        -- TODO: check for valid promotion
        PrimCast PrimCastPromote
         | [XType tTo, XType _tFrom, x1] <- xs
         -> do  tTo'    <- convTypeM   tTo
                x1'     <- convRValueM x1
                return  $  parens tTo' <> parens x1'

        -- TODO: check for valid truncate
        PrimCast PrimCastTruncate
         | [XType tTo, XType _tFrom, x1] <- xs
         -> do  tTo'    <- convTypeM   tTo
                x1'     <- convRValueM x1
                return  $  parens tTo' <> parens x1'


        -- Control primops.
        PrimControl PrimControlReturn
         | [XType _t, x1]       <- xs
         -> do  x1'     <- convRValueM x1
                return  $ text "return" <+> x1'

        PrimControl PrimControlFail
         | [XType _t]           <- xs
         -> do  return  $ text "_fail()"


        -- Store primops.
        PrimStore op
         -> do  let op'  = convPrimStore op
                xs'     <- mapM convRValueM xs
                return  $ op' <+> parenss xs'


        -- External primops.
        PrimExternal op 
         -> do  let op' = convPrimExternal op
                xs'     <- mapM convRValueM xs
                return  $ op' <+> parenss xs'

        _ -> error ("invalid primitive call" ++ show (p, xs))


parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs


-- Prims ----------------------------------------------------------------------
-- | Convert an binary arithmetic primop name to C source text.
convPrimOp2 :: PrimOp -> Maybe Doc
convPrimOp2 pp
 = case pp of
        -- arithmetic   
        PrimOpNeg               -> Just $ text "-"
        PrimOpAdd               -> Just $ text "+"
        PrimOpSub               -> Just $ text "-"
        PrimOpMul               -> Just $ text "*"
        PrimOpDiv               -> Just $ text "/"
        PrimOpRem               -> Just $ text "%"

        -- comparison
        PrimOpEq                -> Just $ text "=="
        PrimOpNeq               -> Just $ text "!="
        PrimOpGt                -> Just $ text ">"
        PrimOpGe                -> Just $ text ">="
        PrimOpLt                -> Just $ text "<"
        PrimOpLe                -> Just $ text "<="

        -- boolean
        PrimOpAnd               -> Just $ text "&&"
        PrimOpOr                -> Just $ text "||"

        -- bitwise
        PrimOpShl               -> Just $ text "<<"
        PrimOpShr               -> Just $ text ">>"
        PrimOpBAnd              -> Just $ text "&"
        PrimOpBOr               -> Just $ text "|"
        PrimOpBXOr              -> Just $ text "^"


-- | Convert a store primop name to C source text.
convPrimStore :: PrimStore -> Doc
convPrimStore pp
 = case pp of
        PrimStoreAlloc          -> text "_alloc"
        PrimStoreRead           -> text "_read"
        PrimStoreWrite          -> text "_write"
        PrimStorePlusAddr       -> text "_plusAddr"
        PrimStoreMinusAddr      -> text "_minusAddr"
        PrimStorePeek           -> text "_peek"
        PrimStorePoke           -> text "_poke"
        PrimStorePlusPtr        -> text "_plusPtr"
        PrimStoreMinusPtr       -> text "_minusPtr"
        PrimStoreMakePtr        -> text "_makePtr"
        PrimStoreTakePtr        -> text "_takePtr"
        PrimStoreCastPtr        -> text "_castPtr"


-- | Convert an external primop name to C source text.
convPrimExternal :: PrimExternal -> Doc
convPrimExternal pp
 = case pp of
        PrimExternalShowInt b   -> text "_showInt" <> int b
        PrimExternalPutStr      -> text "_putStr"
        PrimExternalPutStrLn    -> text "_putStrLn"


