
-- | Pretty printing for Tetra modules and expressions.
module DDC.Source.Tetra.Pretty
        ( module DDC.Type.Pretty
        , module DDC.Core.Pretty
        , module DDC.Base.Pretty )
where
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Predicates
import DDC.Source.Tetra.DataDef
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import DDC.Core.Pretty
import DDC.Type.Pretty
import DDC.Base.Pretty


-- Module ---------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Module a n) where
 ppr Module
        { moduleName            = name
        , moduleExportedTypes   = _exportedTypes
        , moduleExportedValues  = _exportedValues
        , moduleImportedModules = _importedModules
        , moduleTops            = tops }
  =  text "module" <+> ppr name <+> text "where" <$$> (vcat $ map ppr tops)


-- Top ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Top a n) where
 ppr (TopBind _ b x)
  = let dBind = if isBot (typeOfBind b)
                          then ppr (binderOfBind b)
                          else ppr b
    in  align (  dBind
                <> nest 2 ( breakWhen (not $ isSimpleX x)
                          <> text "=" <+> align (ppr x)))

 ppr (TopData _ (DataDef name params ctors))
  = hsep
        (  [ text "data", ppr name]
        ++ [parens $ ppr b | b <- params]
        ++ [text "where" <+> lbrace])
  <$> indent 8
        (vcat [ ppr (dataCtorName ctor) 
                <+> text ":" 
                <+> (hsep   $ punctuate (text " ->") 
                                $ (  map (pprPrec 6) (dataCtorFieldTypes ctor)
                                  ++ [ ppr           (dataCtorResultType ctor)]))
                <> semi
                        | ctor       <- ctors ])
  <> line
  <> rbrace

-- Exp ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Exp a n) where
 pprPrec d xx
  = {-# SCC "ppr[Exp]" #-}
    case xx of
        XVar  _ u       -> ppr u
        XCon  _ dc      -> ppr dc
        
        XLAM{}
         -> let Just (bs, xBody) = takeXLAMs xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "/\\")) groups)
                 <>  (if      isXLAM    xBody then empty
                      else if isXLam    xBody then line <> space
                      else if isSimpleX xBody then space
                      else    line)
                 <>  ppr xBody

        XLam{}
         -> let Just (bs, xBody) = takeXLams xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "\\")) groups) 
                 <> breakWhen (not $ isSimpleX xBody)
                 <> ppr xBody

        XApp _ x1 x2
         -> pprParen' (d > 10)
         $  pprPrec 10 x1 
                <> nest 4 (breakWhen (not $ isSimpleX x2) 
                           <> pprPrec 11 x2)

        XLet _ lts x
         ->  pprParen' (d > 2)
         $   ppr lts <+> text "in"
         <$> ppr x

        XCase _ x1 [AAlt p x2]
         ->  pprParen' (d > 2)
         $   text "caselet" <+> ppr p 
                <+> nest 2 (breakWhen (not $ isSimpleX x1)
                            <> text "=" <+> align (ppr x1))
                <+> text "in"
         <$> ppr x2

        XCase _ x alts
         -> pprParen' (d > 2) 
         $  (nest 2 $ text "case" <+> ppr x <+> text "of" <+> lbrace <> line
                <> (vcat $ punctuate semi $ map ppr alts))
         <> line 
         <> rbrace

        XCast _ CastSuspend x
         -> pprParen' (d > 2)
         $  text "suspend" <$> ppr x

        XCast _ CastRun x
         -> pprParen' (d > 2)
         $  text "run"     <+> ppr x

        XCast _ cc x
         ->  pprParen' (d > 2)
         $   ppr cc <+> text "in"
         <$> ppr x

        XType    t      -> text "[" <> ppr t <> text "]"
        XWitness w      -> text "<" <> ppr w <> text ">"

        XDefix _ xs
         -> pprParen' (d > 2)
         $  text "DEFIX" <+> hsep (map (pprPrec 11) xs)

        XInfixOp _ str
         -> parens $ text "INFIXOP"  <+> text "\"" <> text str <> text "\""

        XInfixVar _ str
         -> parens $ text "INFIXVAR" <+> text "\"" <> text str <> text "\""


-- Alt ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Alt a n) where
 ppr (AAlt p x)
  = ppr p <+> nest 1 (line <> nest 3 (text "->" <+> ppr x))


-- Cast -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast a n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff   
         -> text "weakeff" <+> brackets (ppr eff)

        CastPurify w
         -> text "purify"  <+> angles   (ppr w)

        CastSuspend
         -> text "suspend"

        CastRun
         -> text "run"


-- Lets -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Lets a n) where
 ppr lts
  = case lts of
        LLet b x
         -> let dBind = if isBot (typeOfBind b)
                          then ppr (binderOfBind b)
                          else ppr b
            in  text "let"
                 <+> align (  dBind
                           <> nest 2 ( breakWhen (not $ isSimpleX x)
                                     <> text "=" <+> align (ppr x)))
        
        LLetRegions [b] []
         -> text "letregion"
                <+> ppr (binderOfBind b)
        
        LLetRegions [b] bs
         -> text "letregion"
                <+> ppr (binderOfBind b)
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bs)

        LLetRegions b []
         -> text "letregions"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) b))

        LLetRegions b bs
         -> text "letregions"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) b))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bs)


-- Binder ---------------------------------------------------------------------
pprBinder   :: Pretty n => Binder n -> Doc
pprBinder bb
 = case bb of
        RName v         -> ppr v
        RAnon           -> text "^"
        RNone           -> text "_"


-- | Print a group of binders with the same type.
pprBinderGroup 
        :: (Pretty n, Eq n) 
        => Doc -> ([Binder n], Type n) -> Doc

pprBinderGroup lam (rs, t)
        = lam <> parens ((hsep $ map pprBinder rs) <+> text ":" <+> ppr t) <> dot


-- Utils ----------------------------------------------------------------------
breakWhen :: Bool -> Doc
breakWhen True   = line
breakWhen False  = space


isSimpleX :: Exp a n -> Bool
isSimpleX xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XType{}         -> True
        XWitness{}      -> True
        XApp _ x1 x2    -> isSimpleX x1 && isAtomX x2
        _               -> False


parens' :: Doc -> Doc
parens' d = lparen <> nest 1 d <> rparen


-- | Wrap a `Doc` in parens if the predicate is true.
pprParen' :: Bool -> Doc -> Doc
pprParen' b c
 = if b then parens' c
        else c
