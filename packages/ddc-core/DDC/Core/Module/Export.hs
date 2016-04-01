
module DDC.Core.Module.Export
        ( ExportSource  (..)
        , takeTypeOfExportSource
        , mapTypeOfExportSource)
where
import DDC.Type.Exp
import Control.DeepSeq


-- | Define thing exported from a module.
data ExportSource n
        -- | A name defined in this module, with an explicit type.
        = ExportSourceLocal   
        { exportSourceLocalName         :: n 
        , exportSourceLocalType         :: Type n }

        -- | A named defined in this module, without a type attached.
        --   We use this version for source language where we infer the type of
        --   the exported thing.
        | ExportSourceLocalNoType
        { exportSourceLocalName         :: n }
        deriving Show


instance NFData n => NFData (ExportSource n) where
 rnf es
  = case es of
        ExportSourceLocal n t           -> rnf n `seq` rnf t
        ExportSourceLocalNoType n       -> rnf n


-- | Take the type of an imported thing, if there is one.
takeTypeOfExportSource :: ExportSource n -> Maybe (Type n)
takeTypeOfExportSource es
 = case es of
        ExportSourceLocal _ t           -> Just t
        ExportSourceLocalNoType{}       -> Nothing


-- | Apply a function to any type in an ExportSource.
mapTypeOfExportSource :: (Type n -> Type n) -> ExportSource n -> ExportSource n
mapTypeOfExportSource f esrc
 = case esrc of
        ExportSourceLocal n t           -> ExportSourceLocal n (f t)
        ExportSourceLocalNoType n       -> ExportSourceLocalNoType n

