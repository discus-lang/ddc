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
import DDC.Core.Module
import DDC.Core.Exp.Annot.Exp
import DDC.Core.Exp.Annot.Pretty
import DDC.Core.Exp.Annot.Compounds
import DDC.Type.DataDef
import DDC.Type.Pretty
import DDC.Base.Pretty
import Data.List
import Prelude          hiding ((<$>))


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
        , moduleImportCaps      = importCaps
        , moduleImportValues    = importValues
        , moduleImportDataDefs  = importData
        , moduleDataDefsLocal   = localData
        , moduleBody            = body }
  = {-# SCC "ppr[Module]" #-}
    let 
        (lts, _)                = splitXLets body
        
        -- Exports --------------------
        dExportTypes
         | null $ exportTypes   = empty
         | otherwise            = (vcat $ map pprExportType  exportTypes)  <> line

        dExportValues
         | null $ exportValues  = empty
         | otherwise            = (vcat $ map pprExportValue exportValues) <> line

        -- Imports --------------------
        dImportTypes
         | null $ importTypes   = empty
         | otherwise            = (vcat $ map pprImportType  importTypes)  <> line

        dImportCaps
         | null $ importCaps    = empty
         | otherwise            = (vcat $ map pprImportCap   importCaps)   <> line

        dImportValues
         | null $ importValues  = empty
         | otherwise            = (vcat $ map pprImportValue importValues) <> line

        docsImportsExports
         -- If we're suppressing imports, then don't print it.
         | modeModuleSuppressImports mode 
         = empty
         
         -- If there are no imports or exports then suppress printint.
         | null exportTypes, null exportValues
         , null importTypes, null importCaps, null importValues
         = empty

         | otherwise            
         = line <> dExportTypes <> dExportValues 
                <> dImportTypes <> dImportCaps <> dImportValues
                
        -- Data Definitions -----
        docsDataImport
         | null importData = empty
         | otherwise
         = line <> vsep  (map (\i -> text "import" <+> (ppr i)) $ importData)

        docsDataLocal
         | null localData = empty
         | otherwise
         = line <> vsep  (map ppr localData)
                  
        pprLts = pprModePrec (modeModuleLets mode) 0

    in  text "module" <+> ppr name 
         <+> docsImportsExports
         <>  docsDataImport
         <>  docsDataLocal
         <>  (case lts of
                []       -> empty
                [LRec[]] -> empty
                _        -> text "with" <$$> (vcat $ map pprLts lts))


-- Exports ----------------------------------------------------------------------------------------
-- | Pretty print an exported type definition.
pprExportType :: (Pretty n, Eq n) => (n, ExportSource n) -> Doc
pprExportType (n, esrc)
 = case esrc of
        ExportSourceLocal _n k
         -> text "export type" <+> padL 10 (ppr n) <+> text ":" <+> ppr k <> semi

        ExportSourceLocalNoType _n 
         -> text "export type" <+> padL 10 (ppr n) <> semi


-- | Pretty print an exported value definition.
pprExportValue :: (Pretty n, Eq n) => (n, ExportSource n) -> Doc
pprExportValue (n, esrc)
 = case esrc of
        ExportSourceLocal _n t
         -> text "export value" <+> padL 10 (ppr n) <+> text ":" <+> ppr t <> semi

        ExportSourceLocalNoType _n
         -> text "export value" <+> padL 10 (ppr n) <> semi


-- Imports ----------------------------------------------------------------------------------------
-- | Pretty print a type import.
pprImportType :: (Pretty n, Eq n) => (n, ImportType n) -> Doc
pprImportType (n, isrc)
 = case isrc of
        ImportTypeAbstract k
         -> text "import foreign abstract type" <> line
         <> indent 8 (ppr n <+> text ":" <+> ppr k <> semi)
         <> line

        ImportTypeBoxed k
         -> text "import foreign boxed type" <> line
         <> indent 8 (ppr n <+> text ":" <+> ppr k <> semi)
         <> line


-- | Pretty print a capability import.
pprImportCap :: (Pretty n, Eq n) => (n, ImportCap n) -> Doc
pprImportCap (n, isrc)
 = case isrc of
        ImportCapAbstract t
         -> text "import foreign abstract capability" <> line
         <> indent 8 (padL 15 (ppr n) <+> text ":" <+> ppr t <> semi)
         <> line


-- | Pretty print a value import.
pprImportValue :: (Pretty n, Eq n) => (n, ImportValue n) -> Doc
pprImportValue (n, isrc)
 = case isrc of
        ImportValueModule _mn _nSrc t Nothing
         ->        text "import value" <+> padL 10 (ppr n) <+> text ":" <+> ppr t <> semi

        ImportValueModule _mn _nSrc t (Just (arityType, arityValue, arityBoxes))
         -> vcat [ text "import value" <+> padL 10 (ppr n) <+> text ":" <+> ppr t <> semi
                 , text "{-# ARITY   " <+> padL 10 (ppr n) 
                                       <+> ppr arityType 
                                       <+> ppr arityValue 
                                       <+> ppr arityBoxes
                                       <+> text "#-}"
                 , empty ]

        ImportValueSea _var t
         -> text "import foreign c value" <> line
         <> indent 8 (padL 15 (ppr n) <+> text ":" <+> ppr t <> semi)
         <> line


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
  


