
-- | Pretty printing for Tetra modules and expressions.
module DDC.Source.Tetra.Pretty
        ( module DDC.Core.Pretty
        , module DDC.Base.Pretty )
where
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Predicates
import DDC.Source.Tetra.DataDef
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import DDC.Core.Pretty
import DDC.Base.Pretty
import Prelude                  hiding ((<$>))


-- Module -----------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Module a n) where
 ppr Module
        { moduleName            = name
        , moduleExportTypes     = _exportedTypes
        , moduleExportValues    = _exportedValues
        , moduleImportModules   = _importedModules
        , moduleImportTypes     = importedTypes
        , moduleImportValues    = importedValues
        , moduleTops            = tops }
  =  text "module" 
        <+> ppr name 
        <>  sImportedTypes
        <>  sImportedValues
        <>   (if null importedTypes && null importedValues
                then space <> text "where" 
                else text "where")
        <$$> (vcat $ map ppr tops)

  where sImportedTypes
         | null importedTypes   = empty
         | otherwise
         = line 
         <> (vcat $ map pprImportType importedTypes) 
         <> line

        sImportedValues
         | null importedValues  = empty
         | otherwise
         = (vcat $ map pprImportValue importedValues)
         <> line


-- Top --------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Top a n) where
 ppr (TopClause _ c) = ppr c

 ppr (TopData _ (DataDef name params ctors))
  = (text "data"
        <+> hsep ( ppr name
                 : map (parens . ppr) params)
        <+> text "where"
        <+> lbrace)
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


-- Exp --------------------------------------------------------------------------------------------
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

        XCase _ x alts
         -> pprParen' (d > 2) 
         $  (nest 2 $ text "case" <+> ppr x <+> text "of" <+> lbrace <> line
                <> (vcat $ punctuate semi $ map ppr alts))
         <> line 
         <> rbrace

        XCast _ CastBox x
         -> pprParen' (d > 2)
         $  text "box"  <$> ppr x

        XCast _ CastRun x
         -> pprParen' (d > 2)
         $  text "run"  <+> ppr x

        XCast _ cc x
         ->  pprParen' (d > 2)
         $   ppr cc <+> text "in"
         <$> ppr x

        XType    _ t    -> text "[" <> ppr t <> text "]"
        XWitness _ w    -> text "<" <> ppr w <> text ">"

        XDefix _ xs
         -> text "[" <> text "DEFIX|" <+> hsep (map (pprPrec 11) xs) <+> text "]"

        XInfixOp _ str
         -> parens $ text "INFIXOP"  <+> text "\"" <> text str <> text "\""

        XInfixVar _ str
         -> parens $ text "INFIXVAR" <+> text "\"" <> text str <> text "\""


-- Lets -------------------------------------------------------------------------------------------
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

        LPrivate bs Nothing []
         -> text "private"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))
        
        LPrivate bs Nothing bsWit
         -> text "private"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bsWit)

        LPrivate bs (Just parent) []
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))

        LPrivate bs (Just parent) bsWit
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bsWit)

        LGroup cs
         -> vcat $ map ppr cs


-- Clause -----------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Clause a n) where
 ppr (SSig _ b t)
  = ppr b <+> text ":" <+> ppr t

 ppr (SLet _ b ps [GExp x])
  = let dBind   = if isBot (typeOfBind b)
                        then ppr (binderOfBind b)
                        else ppr b

    in  dBind   <+> hsep (map ppr ps) 
                <>  nest 2 ( breakWhen (not $ isSimpleX x)
                           <> text "=" <+> align (ppr x))

 ppr (SLet _ b ps gxs)
  = let dBind   = if isBot (typeOfBind b)
                        then ppr (binderOfBind b)
                        else ppr b

    in  dBind   <+> hsep (map ppr ps) 
                <>  nest 2 (line <> vcat (map (pprGuardedExp "=") gxs))


-- Alt --------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Alt a n) where
 ppr (AAlt p gxs)
  =  ppr p <> nest 2 (line <> vcat (map (pprGuardedExp "->") gxs))


-- GuardedExp -------------------------------------------------------------------------------------
pprGuardedExp :: (Pretty n, Eq n) => String -> GuardedExp a n -> Doc
pprGuardedExp sTerm gx
  = pprGs "|" gx
  where
        pprGs _c (GExp x)
         = text sTerm <+> ppr x

        pprGs c (GGuard g gs)
         = pprG c g <> line <> pprGs "," gs

        pprG  c (GPat p x)
         = text c <+> ppr p  <+> text "<-" <+> ppr x

        pprG  c (GPred x)
         = text c <+> ppr x

        pprG  c GDefault
         = text c <+> text "otherwise"
        

-- Guard ------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Guard a n) where


-- Cast -------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast a n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff -> text "weakeff" <+> brackets (ppr eff)
        CastPurify w    -> text "purify"  <+> angles   (ppr w)
        CastBox         -> text "box"
        CastRun         -> text "run"


-- Binder -----------------------------------------------------------------------------------------
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


-- Utils ------------------------------------------------------------------------------------------
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
