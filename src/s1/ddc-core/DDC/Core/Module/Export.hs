
module DDC.Core.Module.Export
        ( ExportType  (..)
        , takeKindOfExportType
        , mapKindOfExportType

        , ExportValue  (..)
        , takeTypeOfExportValue
        , mapTypeOfExportValue)
where
import DDC.Core.Module.Name
import Control.DeepSeq
import Data.Text                (Text)


-------------------------------------------------------------------------------
-- | Describe a type exported from a module.
data ExportType n t
        -- | A name defined in this module, with an explicit type.
        = ExportTypeLocal
        { exportTypeLocalName   :: n
        , exportTypeLocalKind   :: t }

        -- | A named defined in this module, without a type attached.
        --   We use this version for source language where we infer the type of
        --   the exported thing.
        | ExportTypeLocalNoKind
        { exportTypeLocalName   :: n }
        deriving Show


instance (NFData n, NFData t) => NFData (ExportType n t) where
 rnf es
  = case es of
        ExportTypeLocal n t     -> rnf n `seq` rnf t
        ExportTypeLocalNoKind n -> rnf n


-- | Take the type of an imported thing, if there is one.
takeKindOfExportType :: ExportType n t -> Maybe t
takeKindOfExportType es
 = case es of
        ExportTypeLocal _ t     -> Just t
        ExportTypeLocalNoKind{} -> Nothing


-- | Apply a function to any type in an ExportSource.
mapKindOfExportType :: (t -> t) -> ExportType n t -> ExportType n t
mapKindOfExportType f esrc
 = case esrc of
        ExportTypeLocal n t     -> ExportTypeLocal n (f t)
        ExportTypeLocalNoKind n -> ExportTypeLocalNoKind n


-------------------------------------------------------------------------------
-- | Describe a value exported from a module.
data ExportValue n t
        -- | A named defined in this module, without a type attached.
        --   We use this version for source language where we infer the type of
        --   the exported thing.
        = ExportValueLocalNoType
        { exportValueLocalName          :: !n }

        -- | A name defined in this module, with an explicit type.
        | ExportValueLocal
        { -- | Name of the module that the value is defined in.
          exportValueLocalModuleName    :: !ModuleName

          -- | Name of the value in the defining module.
        , exportValueLocalName          :: !n

          -- | Type of the value that we're exporting.
        , exportValueLocalType          :: !t

          -- | Attach arity information to the export record so that importers
          --   can avoid looking at the definition of the value if they
          --   just want to call it.
        , exportValueLocalArity         :: !(Maybe (Int, Int, Int)) }

        -- | A value exported via the C calling convention.
        | ExportValueSea
        { -- | Name of the module in which the original value was imported into
          --   or defined in. This is not necessearally the module which we got
          --   the import via.
          exportValueSeaModuleName      :: !ModuleName

          -- | The name we use to refer to the value internally to the module.
        , exportValueSeaNameInternal    :: !n

          -- | The name of the value in the external C name space.
        , exportValueSeaNameExternal    :: !Text

          -- | Type of the exported value.
        , exportValueSeaType            :: !t }
        deriving Show


instance (NFData n, NFData t) => NFData (ExportValue n t) where
 rnf es
  = case es of
        ExportValueLocalNoType n        -> rnf n
        ExportValueLocal mn n t _       -> rnf mn `seq` rnf n `seq` rnf t
        ExportValueSea   mn n _ t       -> rnf mn `seq` rnf n  `seq` rnf t


-- | Take the type of an imported thing, if there is one.
takeTypeOfExportValue :: ExportValue n t -> Maybe t
takeTypeOfExportValue es
 = case es of
        ExportValueLocalNoType{}        -> Nothing
        ExportValueLocal _ _ t _        -> Just t
        ExportValueSea   _ _ _ t        -> Just t


-- | Apply a function to any type in an ExportSource.
mapTypeOfExportValue :: (t -> t) -> ExportValue n t -> ExportValue n t
mapTypeOfExportValue f esrc
 = case esrc of
        ExportValueLocalNoType n        -> ExportValueLocalNoType n
        ExportValueLocal mn n t a       -> ExportValueLocal mn n (f t) a
        ExportValueSea   mn n x t       -> ExportValueSea   mn n x (f t)

