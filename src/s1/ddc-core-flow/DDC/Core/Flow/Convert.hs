{-# LANGUAGE OverloadedStrings #-}
-- | Conversion of Flow to Tetra
--
module DDC.Core.Flow.Convert
        ( tetraOfFlowModule )
where

import DDC.Core.Flow.Convert.Base
import DDC.Core.Flow.Convert.Type
import DDC.Core.Flow.Convert.Exp
import DDC.Core.Exp.Annot
import DDC.Core.Module
import DDC.Control.Check

import qualified DDC.Core.Flow.Prim      as F
import qualified DDC.Core.Salt.Name      as T
import qualified DDC.Core.Salt.Compounds       as T

import DDC.Core.Salt.Convert (initRuntime)
import DDC.Core.Salt.Runtime (Config(..))

import qualified Data.Set               as S
import qualified Data.Text              as T


tetraOfFlowModule :: Module a F.Name -> Either Error (Module a T.Name)
tetraOfFlowModule mm
 = evalCheck (S.empty, S.empty)
 $ convertM  mm

convertM :: Module a F.Name -> ConvertM (Module a T.Name)
convertM mm
  = do
        -- Convert signatures of imported functions.
        tsImportT' <- mapM convertImportNameTypeM  $ moduleImportTypes  mm
        tsImportV' <- mapM convertImportNameValueM $ moduleImportValues mm

        let tsImportV'rest =
              [ ( T.NameVar       "getFieldOfBoxed"
                , ImportValueSea  (T.NameVar "getFieldOfBoxed") "getFieldOfBoxed"
                   $ tForalls [kRegion, kData]
                   $ \[r,d] -> T.tPtr r T.tObj `tFun` T.tNat `tFun` d)

              , ( T.NameVar       "setFieldOfBoxed"
                , ImportValueSea  (T.NameVar "setFieldOfBoxed") "setFieldOfBoxed"
                   $ tForalls [kRegion, kData]
                   $ \[r,d] -> T.tPtr r T.tObj `tFun` T.tNat `tFun` d `tFun` T.tVoid)

              , ( T.NameVar       "allocBoxed"
                , ImportValueSea  (T.NameVar "allocBoxed") "allocBoxed"
                   $ tForalls [kRegion       ]
                   $ \[r  ] -> T.tTag          `tFun` T.tNat `tFun` T.tPtr r T.tObj)
              ]

        -- Convert signatures of exported functions.
        tsExportT' <- mapM convertExportTypeM
                   $  moduleExportTypes  mm

        tsExportV' <- mapM convertExportValueM
                   $  moduleExportValues mm

        -- Convert the body of the module
        body'      <- convertX $ moduleBody mm

        -- Build the output module.
        let mm_tetra
                = ModuleCore
                { moduleName            = moduleName mm
                , moduleIsHeader        = moduleIsHeader mm

                , moduleExportTypes     = tsExportT'
                , moduleExportValues    = tsExportV'

                , moduleImportTypes     = tsImportT'
                , moduleImportCaps      = []
                , moduleImportValues    = tsImportV' ++ tsImportV'rest

                -- We're only using whole module compilation for
                -- flow programs, so there aren't any imports.
                , moduleImportDataDefs  = []
                , moduleImportTypeDefs  = []

                , moduleLocalDataDefs   = []
                , moduleLocalTypeDefs   = []

                , moduleBody            = body' }

        -- Initialise the salt heap.
        -- Hardcode this for now, because eventually this will target tetra.
        mm_init <- case initRuntime (Config 10000)  mm_tetra of
                        Nothing   -> return mm_tetra
                        Just mm'  -> return mm'

        return $ mm_init


---------------------------------------------------------------------------------------------------
-- Convert an export type
convertExportTypeM
        :: (F.Name, ExportType F.Name (Type F.Name))
        -> ConvertM (T.Name, ExportType T.Name (Type T.Name))

convertExportTypeM (_, esrc)
 = case esrc of
        ExportTypeLocal n t
         -> do  n'      <- convertName n
                t'      <- convertType t
                return  $ (n', ExportTypeLocal n' t')

        ExportTypeLocalNoKind n
         -> do  n'      <- convertName n
                return  $ (n', ExportTypeLocalNoKind n')


-- Convert an export source.
convertExportValueM
        :: (F.Name, ExportValue F.Name (Type F.Name))
        -> ConvertM (T.Name, ExportValue T.Name (Type T.Name))

convertExportValueM (_, esrc)
 = case esrc of
        ExportValueLocal n t _
         -> do  n'      <- convertName n
                t'      <- convertType t
                return  $ (n', ExportValueLocal n' t' Nothing)

        ExportValueLocalNoType n
         -> do  n'      <- convertName n
                return  $ (n', ExportValueLocalNoType n')

        ExportValueSea n x t
         -> do  n'      <- convertName n
                t'      <- convertType t
                return  $ (n', ExportValueSea n' x t')


---------------------------------------------------------------------------------------------------
-- | Convert an import spec.
convertImportNameTypeM
        :: (F.Name, ImportType F.Name (Type F.Name))
        -> ConvertM (T.Name, ImportType T.Name (Type T.Name))

convertImportNameTypeM (n, isrc)
 = do   n'      <- convertImportNameM n
        isrc'   <- convertImportTypeM isrc
        return  (n', isrc')


-- | Convert an import spec.
convertImportNameValueM
        :: (F.Name, ImportValue F.Name (Type F.Name))
        -> ConvertM (T.Name, ImportValue T.Name (Type T.Name))

convertImportNameValueM (n, isrc)
 = do   n'      <- convertImportNameM n
        isrc'   <- convertImportValueM isrc
        return  (n', isrc')


-- | Convert an imported name.
--   These can be variable names for values,
--   or variable or constructor names for type imports.
convertImportNameM :: F.Name -> ConvertM T.Name
convertImportNameM n
 = case n of
        F.NameVar str   -> return $ T.NameVar $ T.pack str
        F.NameCon str   -> return $ T.NameCon $ T.pack str
        _               -> throw  $ ErrorInvalidBinder n


-- | Convert an import source.
convertImportTypeM
        :: ImportType F.Name (Type F.Name)
        -> ConvertM (ImportType T.Name (Type T.Name))

convertImportTypeM isrc
 = case isrc of
        ImportTypeAbstract t
         -> do  t'      <- convertType t
                return $ ImportTypeAbstract t'

        ImportTypeBoxed t
         -> do  t'      <- convertType t
                return $ ImportTypeBoxed t'


-- | Convert an import value spec.
convertImportValueM
        :: ImportValue F.Name (Type F.Name)
        -> ConvertM (ImportValue T.Name (Type T.Name))

convertImportValueM isrc
 = case isrc of
        ImportValueModule mn n t _
         -> do  n'      <- convertName n
                t'      <- convertType t
                return  $ ImportValueModule mn n' t' Nothing

        ImportValueSea n str t
         -> do  n'      <- convertName n
                t'      <- convertType t
                return  $ ImportValueSea n' str t'

