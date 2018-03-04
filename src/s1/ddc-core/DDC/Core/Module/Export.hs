
module DDC.Core.Module.Export
        ( ExportType  (..)
        , takeKindOfExportType
        , mapKindOfExportType

        , ExportValue  (..)
        , takeTypeOfExportValue
        , mapTypeOfExportValue)
where
import Control.DeepSeq


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
        -- | A name defined in this module, with an explicit type.
        = ExportValueLocal
        { exportValueLocalName  :: n
        , exportValueLocalType  :: t

          -- | Attach arity information to the export record so that importers
          --   can avoid looking at the definition of the value if they
          --   just want to call it.
        , exportValueLocalArity :: !(Maybe (Int, Int, Int)) }

        -- | A named defined in this module, without a type attached.
        --   We use this version for source language where we infer the type of
        --   the exported thing.
        | ExportValueLocalNoType
        { exportValueLocalName  :: n }
        deriving Show


instance (NFData n, NFData t) => NFData (ExportValue n t) where
 rnf es
  = case es of
        ExportValueLocal n t _          -> rnf n `seq` rnf t
        ExportValueLocalNoType n        -> rnf n


-- | Take the type of an imported thing, if there is one.
takeTypeOfExportValue :: ExportValue n t -> Maybe t
takeTypeOfExportValue es
 = case es of
        ExportValueLocal _ t  _         -> Just t
        ExportValueLocalNoType{}        -> Nothing


-- | Apply a function to any type in an ExportSource.
mapTypeOfExportValue :: (t -> t) -> ExportValue n t -> ExportValue n t
mapTypeOfExportValue f esrc
 = case esrc of
        ExportValueLocal n t a          -> ExportValueLocal n (f t) a
        ExportValueLocalNoType n        -> ExportValueLocalNoType n

