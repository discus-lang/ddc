
-- | Conversion of Flow to Tetra
--
module DDC.Core.Flow.Convert
        ( tetraOfFlowModule )
where

import DDC.Core.Flow.Convert.Base
import DDC.Core.Flow.Convert.Type
import DDC.Core.Flow.Convert.Exp
import DDC.Core.Module
import DDC.Control.Monad.Check

import qualified DDC.Core.Flow.Prim      as F
import qualified DDC.Core.Tetra.Prim     as T


tetraOfFlowModule :: Module a F.Name -> Either Error (Module a T.Name)
tetraOfFlowModule mm
 = evalCheck ()
 $ convertM  mm

convertM :: Module a F.Name -> ConvertM (Module a T.Name)
convertM mm
  = do  
        -- Convert signatures of imported functions.
        tsImportT' <- mapM convertImportM
                   $  moduleImportTypes  mm

        tsImportV' <- mapM convertImportM
                   $  moduleImportValues mm

        -- Convert signatures of exported functions.
        tsExportT' <- mapM convertExportM
                   $  moduleExportTypes  mm

        tsExportV' <- mapM convertExportM
                   $  moduleExportValues mm

        -- Convert the body of the module
        body'      <- convertX $ moduleBody mm

        -- Build the output module.
        let mm_tetra 
                = ModuleCore
                { moduleName           = moduleName mm
                , moduleIsHeader       = moduleIsHeader mm

                , moduleExportTypes    = tsExportT'
                , moduleExportValues   = tsExportV'

                , moduleImportTypes    = tsImportT'
                , moduleImportValues   = tsImportV'

                -- TODO
                , moduleImportDataDefs = []
                , moduleDataDefsLocal  = []

                , moduleBody           = body' }

        return $ mm_tetra


---------------------------------------------------------------------------------------------------
-- | Convert an export spec.
convertExportM
        :: (F.Name, ExportSource F.Name)                
        -> ConvertM (T.Name, ExportSource T.Name)

convertExportM (n, esrc)
 = do   n'      <- convertName n
        esrc'   <- convertExportSourceM esrc
        return  (n', esrc')


-- Convert an export source.
convertExportSourceM 
        :: ExportSource F.Name
        -> ConvertM (ExportSource T.Name)

convertExportSourceM esrc
 = case esrc of
        ExportSourceLocal n t
         -> do  n'      <- convertName n
                t'      <- convertType t
                return  $ ExportSourceLocal n' t'

        ExportSourceLocalNoType n
         -> do  n'      <- convertName n
                return  $ ExportSourceLocalNoType n'


---------------------------------------------------------------------------------------------------
-- | Convert an import spec.
convertImportM
        :: (F.Name, ImportSource F.Name)
        -> ConvertM (T.Name, ImportSource T.Name)

convertImportM (n, isrc)
 = do   n'      <- convertImportNameM n
        isrc'   <- convertImportSourceM isrc
        return  (n', isrc')


-- | Convert an imported name.
--   These can be variable names for values, 
--   or variable or constructor names for type imports.
convertImportNameM :: F.Name -> ConvertM T.Name
convertImportNameM n
 = case n of
        F.NameVar str   -> return $ T.NameVar str
        F.NameCon str   -> return $ T.NameCon str
        _               -> throw  $ ErrorInvalidBinder n


-- | Convert an import source.
convertImportSourceM 
        :: ImportSource F.Name
        -> ConvertM (ImportSource T.Name)

convertImportSourceM isrc
 = case isrc of
        ImportSourceAbstract t
         -> do  t'      <- convertType t
                return $ ImportSourceAbstract t'

        ImportSourceModule mn n t
         -> do  n'      <- convertName n
                t'      <- convertType t
                return  $ ImportSourceModule mn n' t'

        ImportSourceSea str t
         -> do  t'      <- convertType t 
                return  $ ImportSourceSea str t'


