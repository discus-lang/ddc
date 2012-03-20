
module DDC.Core.Sea.Output.Convert
        (Convert(..))
where
import DDC.Core.Compounds
import DDC.Type.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Sea.Output.Name
import DDC.Base.Pretty


-- | Convert a term that complies with the SeaOutput language profile into
--   a Sea expression.
--   If the term doesn't comply with the profile you may get `error`.
class Convert a where
 convert :: a -> Doc


-- Module ---------------------------------------------------------------------
instance Convert (Module () Name) where
 convert mm@(ModuleCore{})
        | [LRec bxs]    <- moduleLets mm
        = vcat $ map convertTop bxs

        | otherwise
        = error "convert[Module]: module must contain top-level letrecs"


-- Type -----------------------------------------------------------------------
instance Convert (Type Name) where
 convert tt
  = case tt of
        TVar u                          -> convert u
        TCon (TyConBound (UName n _))   -> convert n

        TCon (TyConBound (UPrim (NamePrimTyCon tc) _))
         -> case tc of
                PrimTyConVoid           -> text "void"
                PrimTyConAddr           -> text "addr_t"
                PrimTyConNat            -> text "nat_t"
                PrimTyConTag            -> text "tag_t"
                PrimTyConBool           -> text "bool_t"
                PrimTyConInt bits       -> text "int" <> int bits <> text "_t"
                PrimTyConString         -> text "string_t"
                _                       -> error $ "convert[Type]: " ++ show tt

        TApp (TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConPtr) _))) t2
         -> convert t2 <> text "*"

        TCon (TyConBound (UPrim NameObjTyCon _))
         -> text "Obj"

        _                               -> error ("convert[Type]: sorry" ++ show tt)


-- Top level function ---------------------------------------------------------
convertTop :: (Bind Name, Exp () Name) -> Doc
convertTop (b , xx)
        | BName nTop tTop         <- b 
        , Just (bsParam, xBody)   <- takeXLams xx
        ,  (_, tResult)           <- takeTFunArgResult tTop
        =  (convert tResult 
                <+> convert nTop
                <+> encloseSep lparen rparen (comma <> space) 
                        (map convert bsParam))

        <$$> lbrace <> line
        <>  (indent 8 (convert xBody <> semi)) 
        <>   line   <> rbrace
        <>   line 


        | otherwise
        = error "convertTop: sorry"


-- Exp ------------------------------------------------------------------------
instance Convert (Exp () Name) where
 convert xx 
  = case xx of
        XVar _ (UName n _)
         -> convert n

        XCon _ (UPrim (NameNat n) _)
         -> integer n

        XCon _ (UPrim (NameTag n) _)
         -> integer n

        -- TODO: frag check fully applied primops.
        XApp{}
         |  Just (NamePrim p, xs)       <- takeXPrimApps xx
         -> convertPrimApp p xs 

        -- TODO: frag check fully applied calls.
        XApp{}
         |  (XVar _ (UName n _) : args) <- takeXApps xx
         -> convertFunApp n args

        XLet _ (LLet LetStrict (BNone _) x) x2
         -> vcat [ convert x <> semi
                 , convert x2 ]

        XLet _ (LLet LetStrict b x) x2
         -> vcat [ fill 12 (convert b) <+> text "=" <+> convert x <> semi
                 , convert x2 ]

        -- TODO: frag check that we only switch on Nats and Tags.
        -- Special case tag check to make output easier to read.
        XCase _ x [ AAlt (PData u []) x1
                  , AAlt PDefault 
                         xFail@(XApp _ 
                                 (XVar _ (UPrim (NamePrim (PrimControl PrimControlFail)) _)) _)]
         -> vcat [ text "if" 
                        <+> parens (convert x <+> text "!=" <+> convert u)
                        <+> convert xFail
                        <> semi
                 , convert x1 ]

        -- Special case if-then-else to make output easier to read.
        XCase _ x [ AAlt (PData (UPrim (NameBool True)  _) []) x1
                  , AAlt (PData (UPrim (NameBool False) _) []) x2 ]
         -> vcat [ text "if" <+> parens (convert x)
                 , lbrace    <>  (indent 7 $ convert x1) <> semi <> line <> rbrace
                 , lbrace    <>  (indent 7 $ convert x2) <> semi <> line <> rbrace ]

        XCase _ x alts
         ->   text "switch" <> parens (convert x) 
         <$$> lbrace <> (indent 1 (vcat $ map convert alts))
         <$$> rbrace 


        _ -> error $ unlines
                   [ "convert[Exp] failed"      
                   , show xx ]
 

-- | Convert a primop application to Sea.
convertPrimApp :: Prim -> [Exp () Name] -> Doc
convertPrimApp p xs
        -- binary primops
        | PrimOp op             <- p
        , [XType _t, x1, x2]    <- xs
        , elem op [ PrimOpAdd, PrimOpSub, PrimOpMul, PrimOpDiv, PrimOpMod
                  , PrimOpEq,  PrimOpNeq
                  , PrimOpGt,  PrimOpLt,  PrimOpGe, PrimOpLe]
        = parensConvertX x1 <+> convert op <+> parensConvertX x2


        -- Control
        | PrimControl PrimControlReturn <- p
        , [XType _t, x]                 <- xs
        = text "return" <+> convert x

        | PrimControl PrimControlFail   <- p
        , [XType _t]                    <- xs
        = text "_fail()"

        -- Cast
        | PrimCast (PrimCastNatToInt bits) <- p
        , [x1]                          <- xs
        = parens (text "int" <> int bits <> text "_t") 
                <> parens (convert x1)

        -- Store 
        | PrimStore PrimStoreRead       <- p
        , [XType _t, x]                 <- xs
        = text "_read"  <+> parens (convert x)

        | PrimStore PrimStoreProjTag    <- p
        = text "_tag"   <+> convertArgs xs

        | PrimStore (PrimStoreProjField PrimStoreLayoutRaw) <- p
        , [XType t, x1, x2]             <- xs
        =   text "_fieldRaw" 
        <+> encloseSep lparen rparen (comma <> space)
                [ convert t
                , convert x1
                , convert x2]

        | PrimStore (PrimStoreAllocData PrimStoreLayoutRaw) <- p
        , [xTag, xArity]                <- xs
        =   text "_allocRaw" <+> convertArgs [xTag, xArity]

        -- Stmt
        | PrimStmt PrimStmtWrite      <- p
        , [XType _t, x1, x2]            <- xs
        = text "_write" <+> convertArgs [x1, x2]


        -- String
        | PrimString op         <- p
        = convert op <+> convertArgs xs

        -- IO
        | PrimIO op             <- p
        = convert op <+> convertArgs xs


        | otherwise 
        = error $ unlines 
                [ "convertPrimApp failed"
                , show (p, xs) ]


-- | Convert a function application to Sea.
convertFunApp :: Name -> [Exp () Name] -> Doc
convertFunApp n xs
        | NameVar str           <- n
        =   text str
        <+> encloseSep lparen rparen (comma <> space)
                (map convert xs)

        | otherwise
        = error $ unlines
                [ "convertFunApp failed"
                , show (n, xs) ]

parensConvertX xx
 = case xx of
        XVar{}  -> convert xx
        _       -> parens (convert xx)


convertArgs :: [Exp () Name] -> Doc
convertArgs xs
 = encloseSep lparen rparen (comma <> space)
                (map convert xs)


-- Alt ------------------------------------------------------------------------
-- TODO: frag check result of alts is always Void#
instance Convert (Alt () Name) where
 convert alt
  = case alt of
        AAlt PDefault x 
         -> text "default:" 
                <+> convert x <> semi

        AAlt (PData u []) x
         -> vcat [ text "case" <+> convert u <> colon
                 , lbrace 
                        <> indent 5 (vcat [ convert x    <> semi]) 
                        <> line
                        <> rbrace 
                        <> empty 
                 ]

        AAlt{}
         -> error "convert[Alt]: sorry"


-- Bind and Bound -------------------------------------------------------------
instance Convert (Bind Name) where
 convert bb
  = case bb of
        BName n t       -> convert t <+> convert n
        _               -> error "convert[Bind]: sorry"

instance Convert (Bound Name) where
 convert uu
  = case uu of
        UName n _       -> convert n
        UPrim n _       -> convert n
        _               -> error "convert[Bound]: sorry"


-- Names ----------------------------------------------------------------------
instance Convert Name where
 convert nn
  = case nn of
        NamePrim p      -> convert p
        NameVar  str    -> text str
        NameTag  i      -> integer i
        NameNat  i      -> integer i
        NameBool True   -> int 1
        NameBool False  -> int 0
        _               -> error ("convert[Name]: sorry" ++ show nn)


-- Prims ----------------------------------------------------------------------
instance Convert Prim where
 convert nn
  = case nn of
        PrimOp op       -> convert op
        _               -> error ("convert[Prim]: sorry" ++ show nn)


instance Convert PrimOp where
 convert nn
  = case nn of
        -- arithmetic   
        PrimOpNeg       -> text "-"
        PrimOpAdd       -> text "+"
        PrimOpSub       -> text "-"
        PrimOpMul       -> text "*"
        PrimOpDiv       -> text "/"
        PrimOpMod       -> text "%"

        -- comparison
        PrimOpEq        -> text "=="
        PrimOpNeq       -> text "!="
        PrimOpGt        -> text ">"
        PrimOpGe        -> text ">="
        PrimOpLt        -> text "<"
        PrimOpLe        -> text "<="

        -- boolean
        PrimOpAnd       -> text "&&"
        PrimOpOr        -> text "||"


instance Convert PrimString where
 convert nn 
  = case nn of
        PrimStringShowInt b     -> text "_showInt" <> int b

instance Convert PrimIO where
 convert nn 
  = case nn of
        PrimIOPutStr            -> text "_putStr"
        PrimIOPutStrLn          -> text "_putStrLn"
