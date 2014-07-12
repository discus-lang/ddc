{-# LANGUAGE TypeFamilies #-}

-- | Pretty printing for core modules and expressions.
module DDC.Core.Pretty 
        ( module DDC.Type.Pretty
        , module DDC.Base.Pretty
        , PrettyMode (..)
        , pprExportType
        , pprExportValue
        , pprImportType
        , pprImportValue)
where
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.DataDef
import DDC.Type.Pretty
import DDC.Base.Pretty
import Data.List


-- ModuleName -------------------------------------------------------------------------------------
instance Pretty ModuleName where
 ppr (ModuleName parts)
        = text $ intercalate "." parts


-- Module -----------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Module a n) where
 data PrettyMode (Module a n)
        = PrettyModeModule
        { modeModuleLets                :: PrettyMode (Lets a n)
        , modeModuleSuppressImports     :: Bool 
        , modeModuleSuppressExports     :: Bool }

 pprDefaultMode
        = PrettyModeModule
        { modeModuleLets                = pprDefaultMode
        , modeModuleSuppressImports     = False
        , modeModuleSuppressExports     = False }

 pprModePrec mode _
        ModuleCore 
        { moduleName            = name
        , moduleExportTypes     = exportTypes
        , moduleExportValues    = exportValues
        , moduleImportTypes     = importTypes
        , moduleImportValues    = importValues
        , moduleDataDefsLocal   = localData
        , moduleBody            = body }
  = {-# SCC "ppr[Module]" #-}
    let 
        (lts, _)                = splitXLets body
        
        -- Exports --------------------
        dExportTypes
         | null $ exportTypes   = empty
         | otherwise            = (vcat $ map pprExportType  exportTypes)   <> line

        dExportValues
         | null $ exportValues  = empty
         | otherwise            = (vcat $ map pprExportValue exportValues) <> line

        -- Imports --------------------
        dImportTypes
         | null $ importTypes   = empty
         | otherwise            = (vcat $ map pprImportType importTypes)   <> line

        dImportValues
         | null $ importValues  = empty
         | otherwise            = (vcat $ map pprImportValue importValues) <> line

        docsImportsExports
         -- If we're suppressing imports, then don't print it.
         | modeModuleSuppressImports mode 
         = empty
         
         -- If there are no imports or exports then suppress printint.
         | null exportTypes, null exportValues, null importTypes, null importValues
         = empty

         | otherwise            
         = line <> dExportTypes <> dExportValues <> dImportTypes <> dImportValues
                
        -- Local Data Definitions -----
        docsLocalData
         | null localData = empty
         | otherwise
         = line <> vsep  (map ppr localData)
                  
        pprLts = pprModePrec (modeModuleLets mode) 0

    in  text "module" <+> ppr name 
         <+> docsImportsExports
         <>  docsLocalData
         <>  (case lts of
                []      -> empty
                _       -> text "with" <$$> (vcat $ map pprLts lts))


-- Exports ----------------------------------------------------------------------------------------
-- | Pretty print an exported type definition.
pprExportType :: (Pretty n, Eq n) => (n, ExportSource n) -> Doc
pprExportType (n, esrc)
 = case esrc of
        ExportSourceLocal _n k
         -> text "export type" <+> ppr n  <+> text ":" <+> ppr k <> semi

        ExportSourceLocalNoType _n 
         -> text "export type" <+> ppr n  <> semi


-- | Pretty print an exported value definition.
pprExportValue :: (Pretty n, Eq n) => (n, ExportSource n) -> Doc
pprExportValue (n, esrc)
 = case esrc of
        ExportSourceLocal _n t
         -> text "export value" <+> ppr n  <+> text ":" <+> ppr t <> semi

        ExportSourceLocalNoType _n
         -> text "export value" <+> ppr n  <> semi


-- Imports ----------------------------------------------------------------------------------------
-- | Pretty print an imported type definition.                
pprImportType :: (Pretty n, Eq n) => (n, ImportSource n) -> Doc
pprImportType (n, isrc)
 = case isrc of
        ImportSourceModule _mn _nSrc k
         -> text "import type" <+> ppr n <+> text ":" <+> ppr k <> semi

        ImportSourceAbstract k
         -> text "import foreign abstract type" <> line
         <> indent 8 (ppr n <+> text ":" <+> ppr k <> semi)

        ImportSourceSea _var k
         -> text "import foreign c type" <> line
         <> indent 8 (ppr n <+> text ":" <+> ppr k <> semi)


-- | Pretty print an imported value definition.
pprImportValue :: (Pretty n, Eq n) => (n, ImportSource n) -> Doc
pprImportValue (n, isrc)
 = case isrc of
        ImportSourceModule _mn _nSrc t
         -> text "import value" <+> ppr n <+> text ":" <+> ppr t <> semi

        ImportSourceAbstract t
         -> text "import foreign abstract value" <> line
         <> indent 8 (ppr n <+> text ":" <+> ppr t <> semi)

        ImportSourceSea _var t
         -> text "import foreign c value" <> line
         <> indent 8 (ppr n <+> text ":" <+> ppr t <> semi)


-- DataDef ----------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DataDef n) where
 pprPrec _ def
  = {-# SCC "ppr[DataDef]" #-}
      (text "data" 
        <+> hsep ( ppr (dataDefTypeName def)
                 : map (parens . ppr) (dataDefParams def))
        <+> text "where"
        <+>  lbrace)
  <$> (case dataDefCtors def of
        Just ctors
         -> indent 8
          $ vcat [ ppr ctor <> semi | ctor <- ctors]

        Nothing
         -> text "LARGE")
  <> line
  <> rbrace
  <> line


-- DataCtor ---------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DataCtor n) where
 pprPrec _ ctor
        =   ppr (dataCtorName ctor)
        <+> text ":"
        <+> (hsep $ punctuate (text " ->") 
                  $ (map (pprPrec 6) 
                        (  dataCtorFieldTypes ctor
                        ++ [dataCtorResultType ctor])))
  

-- Exp --------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Exp a n) where
 data PrettyMode (Exp a n)
        = PrettyModeExp
        { modeExpLets           :: PrettyMode (Lets a n)
        , modeExpAlt            :: PrettyMode (Alt a n)
        
          -- Display types on primitive variables.
        , modeExpVarTypes       :: Bool

          -- Display types on primitive constructors.
        , modeExpConTypes       :: Bool
        
          -- Use 'letcase' for single alternative case expressions.
        , modeExpUseLetCase     :: Bool }


 pprDefaultMode
        = PrettyModeExp
        { modeExpLets           = pprDefaultMode
        , modeExpAlt            = pprDefaultMode
        , modeExpConTypes       = False
        , modeExpVarTypes       = False
        , modeExpUseLetCase     = False }


 pprModePrec mode d xx
  = let pprX    = pprModePrec mode 0
        pprLts  = pprModePrec (modeExpLets mode) 0
        pprAlt  = pprModePrec (modeExpAlt  mode) 0
    in case xx of

        XVar  _ u       
         | modeExpVarTypes mode
         , Just t       <- takeTypeOfBound u
         -> parens $ ppr u <> text " : " <> ppr t

         | otherwise
         -> ppr u


        XCon  _ dc
         | modeExpConTypes mode
         , Just t       <- takeTypeOfDaCon dc
         -> parens $ ppr dc <> text " : " <> ppr t
        
         | otherwise
         -> ppr dc
        
        
        XLAM{}
         -> let Just (bs, xBody) = takeXLAMs xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "/\\")) groups)
                 <>  (if      isXLAM    xBody then empty
                      else if isXLam    xBody then line <> space
                      else if isSimpleX xBody then space
                      else    line)
                 <>  pprX xBody

        XLam{}
         -> let Just (bs, xBody) = takeXLams xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "\\")) groups) 
                 <> breakWhen (not $ isSimpleX xBody)
                 <> pprX xBody

        XApp _ x1 x2
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1 
                <> nest 4 (breakWhen (not $ isSimpleX x2) 
                          <> pprModePrec mode 11 x2)

        XLet _ lts x
         ->  pprParen' (d > 2)
         $   pprLts lts <+> text "in"
         <$> pprX x

        -- Print single alternative case expressions as 'letcase'.
        --    case x1 of { C v1 v2 -> x2 }
        -- => letcase C v1 v2 <- x1 in x2
        XCase _ x1 [AAlt p x2]
         | modeExpUseLetCase mode
         ->  pprParen' (d > 2)
         $   text "letcase" <+> ppr p 
                <+> nest 2 (breakWhen (not $ isSimpleX x1)
                            <> text "=" <+> align (pprX x1))
                <+> text "in"
         <$> pprX x2

        XCase _ x alts
         -> pprParen' (d > 2) 
         $  (nest 2 $ text "case" <+> ppr x <+> text "of" <+> lbrace <> line
                <> (vcat $ punctuate semi $ map pprAlt alts))
         <> line 
         <> rbrace

        XCast _ CastBox x
         -> pprParen' (d > 2)
         $  text "box"  <$> pprX x

        XCast _ CastRun x
         -> pprParen' (d > 2)
         $  text "run"  <+> pprX x

        XCast _ cc x
         ->  pprParen' (d > 2)
         $   ppr cc <+> text "in"
         <$> pprX x

        XType    _ t    -> text "[" <> ppr t <> text "]"
        XWitness _ w    -> text "<" <> ppr w <> text ">"


-- Pat --------------------------------------------------------------------------------------------
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


-- Alt --------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Alt a n) where
 data PrettyMode (Alt a n)
        = PrettyModeAlt
        { modeAltExp            :: PrettyMode (Exp a n) }

 pprDefaultMode
        = PrettyModeAlt
        { modeAltExp            = pprDefaultMode }

 pprModePrec mode _ (AAlt p x)
  = let pprX    = pprModePrec (modeAltExp mode) 0
    in  ppr p <+> nest 1 (line <> nest 3 (text "->" <+> pprX x))


-- DaCon ------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DaCon n) where
 ppr dc
  = case dc of
        DaConUnit       -> text "()"
        DaConPrim  n _  -> ppr n
        DaConBound n    -> ppr n


-- Cast -------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast a n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff   
         -> text "weakeff" <+> brackets (ppr eff)

        CastWeakenClosure xs
         -> text "weakclo" 
         <+> braces (hcat $ punctuate (semi <> space) 
                          $ map ppr xs)

        CastPurify w
         -> text "purify"  <+> angles   (ppr w)

        CastForget w
         -> text "forget"  <+> angles   (ppr w)

        CastBox
         -> text "box"

        CastRun
         -> text "run"


-- Lets -------------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Lets a n) where
 data PrettyMode (Lets a n)
        = PrettyModeLets
        { modeLetsExp           :: PrettyMode (Exp a n) 
        , modeLetsSuppressTypes :: Bool }

 pprDefaultMode
        = PrettyModeLets
        { modeLetsExp           = pprDefaultMode 
        , modeLetsSuppressTypes = False }

 pprModePrec mode _ lts
  = let pprX    = pprModePrec (modeLetsExp mode) 0
    in case lts of
        LLet b x
         -> let dBind = if  isBot (typeOfBind b)
                         || modeLetsSuppressTypes mode
                          then ppr (binderOfBind b)
                          else ppr b
            in  text "let"
                 <+> align (  dBind
                           <> nest 2 ( breakWhen (not $ isSimpleX x)
                                     <> text "=" <+> align (pprX x)))

        LRec bxs
         -> let pprLetRecBind (b, x)
                 =   ppr (binderOfBind b)
                 <+> text ":"
                 <+> ppr (typeOfBind b)
                 <>  nest 2 (  breakWhen (not $ isSimpleX x)
                            <> text "=" <+> align (pprX x))
        
           in   (nest 2 $ text "letrec"
                  <+> lbrace 
                  <>  (  line 
                      <> (vcat $ punctuate (semi <> line)
                               $ map pprLetRecBind bxs)))
                <$> rbrace

        LPrivate bs Nothing []
         -> text "private"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))

        LPrivate bs Nothing bws
         -> text "private"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map pprWitBind bws)

        LPrivate bs (Just parent) []
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))

        LPrivate bs (Just parent) bws
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map pprWitBind bws)
        
        LWithRegion b
         -> text "withregion"
                <+> ppr b


-- | When we pretty print witness binders, 
--   suppress the underscore when there is no name.
pprWitBind :: (Eq n, Pretty n) => Bind n -> Doc
pprWitBind b
 = case b of
        BNone t -> ppr t
        _       -> ppr b


-- Witness ----------------------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Witness a n) where
 pprPrec d ww
  = case ww of
        WVar _ n         -> ppr n
        WCon _ wc        -> ppr wc

        WApp _ w1 w2
         -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
         
        WJoin _ w1 w2
         -> pprParen (d > 9)  (ppr w1 <+> text "&" <+> ppr w2)

        WType _ t        -> text "[" <> ppr t <> text "]"


instance (Pretty n, Eq n) => Pretty (WiCon n) where
 ppr wc
  = case wc of
        WiConBuiltin wb   -> ppr wb
        WiConBound   u  _ -> ppr u


instance Pretty WbCon where
 ppr wb
  = case wb of
        WbConPure       -> text "pure"
        WbConEmpty      -> text "empty"
        WbConUse        -> text "use"
        WbConRead       -> text "read"
        WbConAlloc      -> text "alloc"


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
        =  lam 
        <> parens ((hsep $ map pprBinder rs) <+> text ":" <+> ppr t) 
        <> dot


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
