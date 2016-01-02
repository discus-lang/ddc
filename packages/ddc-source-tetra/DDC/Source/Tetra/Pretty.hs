{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Pretty printing for Tetra modules and expressions.
module DDC.Source.Tetra.Pretty
        ( module DDC.Core.Pretty
        , module DDC.Base.Pretty 
        , PrettyLanguage        (..))
where
import DDC.Source.Tetra.Predicates
import DDC.Source.Tetra.DataDef
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import DDC.Core.Pretty
import DDC.Base.Pretty
import Prelude                  hiding ((<$>))


type PrettyLanguage l 
        = ( Eq     (GName l)
          , Pretty (GAnnot l), Pretty (GName l)
          , Pretty (GBound l), Pretty (GBind l), Pretty (GPrim l))


-- Module -----------------------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (Module l) where
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
instance PrettyLanguage l => Pretty (Top l) where
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
instance PrettyLanguage l => Pretty (GExp l) where
 pprPrec d xx
  = {-# SCC "ppr[Exp]" #-}
    case xx of
        XVar  _ u       -> ppr u
        XCon  _ dc      -> ppr dc
        XPrim _ u       -> ppr u
        
        XLAM _ b xBody
         -> pprParen' (d > 1)
                 $   text "/\\" <>  ppr b <> text "."
                 <>  (if      isXLAM    xBody then empty
                      else if isXLam    xBody then line <> space
                      else if isSimpleX xBody then space
                      else    line)
                 <>  ppr xBody

        XLam _ b xBody
         -> pprParen' (d > 1)
                 $  text "\\" <> ppr b <> text "."
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
instance PrettyLanguage l => Pretty (GLets l) where
 ppr lts
  = case lts of
        LLet b x
         -> text "let"
                 <+> align (  ppr b
                           <> nest 2 ( breakWhen (not $ isSimpleX x)
                                     <> text "=" <+> align (ppr x)))
        LRec bxs
         -> let pprLetRecBind (b, x)
                 =   ppr b
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
                <+> (hcat $ punctuate space (map ppr bs))
        
        LPrivate bs Nothing bsWit
         -> text "private"
                <+> (hcat $ punctuate space (map ppr bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bsWit)

        LPrivate bs (Just parent) []
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map ppr bs))

        LPrivate bs (Just parent) bsWit
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map ppr bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bsWit)

        LGroup cs
         -> vcat $ map ppr cs


-- Clause -----------------------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GClause l) where
 ppr (SSig _ b t)
  = ppr b <+> text ":" <+> ppr t

 ppr (SLet _ b ps [GExp x])
  = ppr b       <+> hsep (map ppr ps) 
                <>  nest 2 ( breakWhen (not $ isSimpleX x)
                           <> text "=" <+> align (ppr x))

 ppr (SLet _ b ps gxs)
  = ppr b       <+> hsep (map ppr ps) 
                <>  nest 2 (line <> vcat (map (pprGuardedExp "=") gxs))


-- Alt --------------------------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GAlt l) where
 ppr (AAlt p gxs)
  =  ppr p <> nest 2 (line <> vcat (map (pprGuardedExp "->") gxs))


-- Pat --------------------------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GPat l) where
 ppr pp
  = case pp of
        PDefault        -> text "_"
        PData u bs      -> ppr u <+> sep (map ppr bs)


-- GuardedExp -------------------------------------------------------------------------------------
pprGuardedExp :: PrettyLanguage l => String -> GGuardedExp l -> Doc
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
instance PrettyLanguage l => Pretty (GGuard l) where


-- Cast -------------------------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GCast l) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff -> text "weakeff" <+> brackets (ppr eff)
        CastPurify w    -> text "purify"  <+> angles   (ppr w)
        CastBox         -> text "box"
        CastRun         -> text "run"


-- Witness ----------------------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GWitness l) where
 pprPrec d ww
  = case ww of
        WVar _ n        -> ppr n
        WCon _ wc       -> ppr wc
        WApp _ w1 w2    -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
        WType _ t       -> text "[" <> ppr t <> text "]"


instance PrettyLanguage l => Pretty (GWiCon l) where
 ppr wc
  = case wc of
        WiConBound   u  _ -> ppr u


-- Utils ------------------------------------------------------------------------------------------
breakWhen :: Bool -> Doc
breakWhen True   = line
breakWhen False  = space


isSimpleX :: GExp l -> Bool
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
