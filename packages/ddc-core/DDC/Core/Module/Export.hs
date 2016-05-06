
module DDC.Core.Module.Export
        ( ExportSource  (..)
        , takeTypeOfExportSource
        , mapTypeOfExportSource)
where
import Control.DeepSeq


-- | Define thing exported from a module.
data ExportSource n t
        -- | A name defined in this module, with an explicit type.
        = ExportSourceLocal   
        { exportSourceLocalName         :: n 
        , exportSourceLocalType         :: t }

        -- | A named defined in this module, without a type attached.
        --   We use this version for source language where we infer the type of
        --   the exported thing.
        | ExportSourceLocalNoType
        { exportSourceLocalName         :: n }
        deriving Show


instance (NFData n, NFData t) => NFData (ExportSource n t) where
 rnf es
  = case es of
        ExportSourceLocal n t           -> rnf n `seq` rnf t
        ExportSourceLocalNoType n       -> rnf n


-- | Take the type of an imported thing, if there is one.
takeTypeOfExportSource :: ExportSource n t -> Maybe t
takeTypeOfExportSource es
 = case es of
        ExportSourceLocal _ t           -> Just t
        ExportSourceLocalNoType{}       -> Nothing


-- | Apply a function to any type in an ExportSource.
mapTypeOfExportSource :: (t -> t) -> ExportSource n t -> ExportSource n t
mapTypeOfExportSource f esrc
 = case esrc of
        ExportSourceLocal n t           -> ExportSourceLocal n (f t)
        ExportSourceLocalNoType n       -> ExportSourceLocalNoType n

