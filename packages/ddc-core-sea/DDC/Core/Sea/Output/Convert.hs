
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
        = error "convert[Module]: sorry"


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

        XApp{}
         |  Just (NamePrim p, xs)       <- takeXPrimApps xx
         -> convertPrimApp p xs 

        XApp{}
         |  (XVar _ (UName n _) : args) <- takeXApps xx
         -> convertFunApp n args

        XLet _ (LLet LetStrict (BNone _) x) x2
         -> vcat [ convert x <> semi
                 , convert x2 ]

        XLet _ (LLet LetStrict b x) x2
         -> vcat [ fill 12 (convert b) <+> text "=" <+> convert x <> semi
                 , convert x2 ]

        _ -> error $ unlines
                   [ "convert[Exp] failed"      
                   , show xx ]
 

-- | Convert a primop application to Sea.
convertPrimApp :: Prim -> [Exp () Name] -> Doc
convertPrimApp p xs
        -- binary primops
        | PrimOp op             <- p
        , [XType _t, x1, x2]    <- xs
        , elem op [PrimOpAdd, PrimOpSub, PrimOpMul, PrimOpDiv, PrimOpMod]
        = parensConvertX x1 <+> convert op <+> parensConvertX x2

        -- Control
        | PrimControl PrimControlReturn <- p
        , [XType _t, x]         <- xs
        = text "return" <+> convert x

        -- Cast
        | PrimCast (PrimCastNatToInt bits) <- p
        , [x1]                  <- xs
        = parens (text "int" <> int bits <> text "_t") 
                <> parens (convert x1)

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
        _               -> error "convert[Bound]: sorry"


-- Names ----------------------------------------------------------------------
instance Convert Name where
 convert nn
  = case nn of
        NamePrim p      -> convert p
        NameVar  str    -> text str
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
        PrimStringShowInt b 
         -> text "showInt" <> int b

instance Convert PrimIO where
 convert nn 
  = case nn of
        PrimIOPutStr    -> text "putStr"
