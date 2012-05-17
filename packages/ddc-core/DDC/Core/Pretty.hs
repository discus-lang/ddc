-- | Pretty printing for core expressions.
module DDC.Core.Pretty 
        ( module DDC.Type.Pretty
        , module DDC.Base.Pretty)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Type.Pretty
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Base.Pretty
import Data.List
import qualified Data.Map       as Map


-- ModuleName -----------------------------------------------------------------
instance Pretty ModuleName where
 ppr (ModuleName parts)
        = text $ intercalate "." parts


-- Module ---------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Module a n) where
 ppr ModuleCore 
        { moduleName            = name
        , moduleExportKinds     = _exportKinds
        , moduleExportTypes     = _exportTypes
        , moduleImportKinds     = _importKinds
        , moduleImportTypes     = importTypes
        , moduleBody            = body }
  = let (lts, _)         = splitXLets body

        docsImportTypes  = [ ppr n <+> text "::" <+> ppr t 
                                | (n, (_, t))   <- Map.toList importTypes]

    in  text "module" <+> ppr name 
         <+> (if Map.null importTypes 
                then empty
                else line 
                        <> text "imports" <+> lbrace 
                        <> (nest 8 $ line <> vcat docsImportTypes)
                        <> line 
                        <> rbrace)
         <> text "with" <$$> (vcat $ map ppr lts)


-- Exp ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Exp a n) where
 pprPrec d xx
  = case xx of
        XVar  _ u       -> ppr u
        XCon  _ tc      -> ppr tc
        
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

        XCase _ x alts
         -> pprParen' (d > 2) 
         $  (nest 2 $ text "case" <+> ppr x <+> text "of" <+> lbrace <> line
                <> (vcat $ punctuate semi $ map ppr alts))
         <> line 
         <> rbrace

        XCast _ cc x
         ->  pprParen' (d > 2)
         $   ppr cc <+> text "in"
         <$> ppr x

        XType    t      -> text "[" <> ppr t <> text "]"
        XWitness w      -> text "<" <> ppr w <> text ">"


-- Pat ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Pat n) where
 ppr pp
  = case pp of
        PDefault        -> text "_"
        PData u bs      -> ppr u <+> sep (map pprPatBind bs)


-- | Pretty print a binder, 
--   showing its type annotation only if it's not bottom.
pprPatBind :: (Eq n, Pretty n) => Bind n -> Doc
pprPatBind b
        | isBot (typeOfBind b)  = ppr $ binderOfBind b
        | otherwise             = parens $ ppr b


-- Alt ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Alt a n) where
 ppr (AAlt p x)
  = ppr p <+> nest 1 (line <> nest 3 (text "->" <+> ppr x))


-- Cast -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff   
         -> text "weakeff" <+> brackets (ppr eff)

        CastWeakenClosure clo
         -> text "weakclo" <+> brackets (ppr clo)

        CastPurify w
         -> text "purify"  <+> angles   (ppr w)

        CastForget w
         -> text "forget"  <+> angles   (ppr w)


-- Lets -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Lets a n) where
 ppr lts
  = case lts of
        LLet m b x
         -> let dBind = if isBot (typeOfBind b)
                          then ppr (binderOfBind b)
                          else ppr b
            in  text "let"
                 <+> align (  dBind <> ppr m
                           <> nest 2 ( breakWhen (not $ isSimpleX x)
                                     <> text "=" <+> align (ppr x)))

        LRec bxs
         -> let pprLetRecBind (b, x)
                 =   ppr (binderOfBind b)
                 <+> text ":"
                 <+> ppr (typeOfBind b)
                 <>  nest 2 (  breakWhen (not $ isSimpleX x)
                            <> text "=" <+> align (ppr x))
        
           in   (nest 2 $ text "letrec"
                  <+> lbrace 
                  <>  (  line 
                      <> (vcat $ punctuate (semi <> line)
                               $ map pprLetRecBind bxs)))
                <$> rbrace


        LLetRegion b []
         -> text "letregion"
                <+> ppr (binderOfBind b)

        LLetRegion b bs
         -> text "letregion"
                <+> ppr (binderOfBind b)
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bs)

        LWithRegion b
         -> text "withregion"
                <+> ppr b


instance (Pretty n, Eq n) => Pretty (LetMode n) where
 ppr lm
  = case lm of
        LetStrict        -> empty
        LetLazy Nothing  -> text " lazy"
        LetLazy (Just w) -> text " lazy <" <> ppr w <> text ">"


-- Witness --------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Witness n) where
 pprPrec d ww
  = case ww of
        WVar n          -> ppr n
        WCon wc         -> ppr wc

        WApp w1 w2
         -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
         
        WJoin w1 w2
         -> pprParen (d > 9)  (ppr w1 <+> text "&" <+> ppr w2)

        WType t         -> text "[" <> ppr t <> text "]"


instance (Pretty n, Eq n) => Pretty (WiCon n) where
 ppr wc
  = case wc of
        WiConBuiltin wb -> ppr wb
        WiConBound   u  -> ppr u


instance Pretty WbCon where
 ppr wb
  = case wb of
        WbConPure       -> text "pure"
        WbConEmpty      -> text "empty"
        WbConUse        -> text "use"
        WbConRead       -> text "read"
        WbConAlloc      -> text "alloc"


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
