{-# LANGUAGE UndecidableInstances #-}

-- | Definition of Source Tetra modules.
module DDC.Source.Tetra.Module
        ( -- * Modules
          Module        (..)
        , isMainModule
        , ExportSource  (..)
        , ImportType    (..)
        , ImportCap     (..)
        , ImportValue   (..)

          -- * Module Names
        , QualName      (..)
        , ModuleName    (..)
        , isMainModuleName

          -- * Top-level things
        , Top           (..)

          -- * Data type definitions
        , DataDef       (..))
where
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.DataDef
import Control.DeepSeq

import DDC.Core.Module          
        ( QualName      (..)
        , ModuleName    (..)
        , isMainModuleName
        , ExportSource  (..)
        , ImportType    (..)
        , ImportCap     (..)
        , ImportValue   (..))
        

-- Module -----------------------------------------------------------------------------------------
data Module l
        = Module
        { -- | Name of this module
          moduleName            :: !ModuleName

          -- Exports ----------------------------
          -- | Names of exported types  (level-1).
        , moduleExportTypes     :: [GTBoundCon l]

          -- | Names of exported values (level-0).
        , moduleExportValues    :: [GXBoundVar l]

          -- Imports ----------------------------
          -- | Imported modules.
        , moduleImportModules   :: [ModuleName]

          -- | Kinds of imported foreign types.
        , moduleImportTypes     :: [(GTBindCon l, ImportType  (GTBindCon l) (GType l))]

          -- | Types of imported capabilities.
        , moduleImportCaps      :: [(GXBindVar l, ImportCap   (GXBindVar l) (GType l))]

          -- | Types of imported foreign values.
        , moduleImportValues    :: [(GXBindVar l, ImportValue (GXBindVar l) (GType l))]

          -- Local ------------------------------
          -- | Top-level things
        , moduleTops            :: [Top l] }


deriving instance ShowLanguage l => Show (Module l)


instance NFDataLanguage l => NFData (Module l) where
 rnf !mm
        =     rnf (moduleName           mm)
        `seq` rnf (moduleExportTypes    mm)
        `seq` rnf (moduleExportValues   mm)
        `seq` rnf (moduleImportModules  mm)
        `seq` rnf (moduleImportTypes    mm)
        `seq` rnf (moduleImportCaps     mm)
        `seq` rnf (moduleImportValues   mm)
        `seq` rnf (moduleTops           mm)
        

-- | Check if this is the `Main` module.
isMainModule :: Module l -> Bool
isMainModule mm
        = isMainModuleName $ moduleName mm


-- Top Level Thing --------------------------------------------------------------------------------
data Top l
        -- | Some top-level, possibly recursive clauses.
        = TopClause  
        { topAnnot      :: GXAnnot l
        , topClause     :: GClause l }

        -- | Data type definition.
        | TopData 
        { topAnnot      :: GXAnnot l
        , topDataDef    :: DataDef l }

deriving instance ShowLanguage l => Show (Top l)

instance NFDataLanguage l => NFData (Top l) where
 rnf !top
  = case top of
        TopClause a c   -> rnf a `seq` rnf c
        TopData   a def -> rnf a `seq` rnf def

