
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
        _                               -> error "convert[Type]: sorry" 


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

        <$$> braces (convert xBody) 


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

        _ -> error "convert: sorry"
 

convertPrimApp :: Prim -> [Exp () Name] -> Doc
convertPrimApp p xs
        | PrimOp op             <- p
        , [XType _t, x1, x2]    <- xs
        , elem op [PrimOpAdd, PrimOpSub, PrimOpMul, PrimOpDiv, PrimOpMod]
        = parensConvertX x1 <+> convert op <+> parensConvertX x2

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
        _               -> error "convert[Name]: sorry"


-- Prims ----------------------------------------------------------------------
instance Convert Prim where
 convert nn
  = case nn of
        PrimOp op       -> convert op
        _               -> error "convert[Prim]: sorry"


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
