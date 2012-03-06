
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
                PrimTyConInt bits       -> text "int" <> int bits <> text "_t"
                _                       -> error "convert[Type]: sort"


        _                               -> text "dunno"
--        _                               -> error ("convert[Type]: sorry" ++ show tt)


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

        <$$> braces (convert xBody <> semi)
        <>   line 


        | otherwise
        = error "convertTop: sorry"


-- Exp ------------------------------------------------------------------------
instance Convert (Exp () Name) where
 convert xx 
  = case xx of
        XVar _ (UName n _)
         -> convert n

        XApp{}
         |  Just (NamePrim p, xs)     <- takeXPrimApps xx
         -> convertPrimApp p xs 

        XLet _ (LLet LetStrict b x) x2
         -> vcat [ convert b <+> text "=" <+> convert x <> semi
                 , convert x2 ]

        _ -> error "convert[Exp]: sorry"
 

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
        = text "return" <+> parens (convert x)


        | otherwise 
        = error "convertPrimApp: sorry"

parensConvertX xx
 = case xx of
        XVar{}  -> convert xx
        _       -> parens (convert xx)


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
