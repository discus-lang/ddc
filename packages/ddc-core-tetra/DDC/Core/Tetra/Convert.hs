-- | Conversion of Disciple Lite to Disciple Salt.
--
module DDC.Core.Tetra.Convert
        ( saltOfTetraModule
        , Error(..))
where
import DDC.Core.Tetra.Convert.Exp
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Base
import DDC.Core.Salt.Convert             (initRuntime)
import DDC.Core.Salt.Platform
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A

import DDC.Type.DataDef
import DDC.Type.Env                      (KindEnv, TypeEnv)
import qualified DDC.Type.Env            as Env

import DDC.Control.Monad.Check           (throw, evalCheck)
import qualified Data.Map                as Map
import qualified Data.Set                as Set


---------------------------------------------------------------------------------------------------
-- | Convert a Core Tetra module to Core Salt.
--
--   The input module needs to be:
--      well typed,
--      fully named with no deBruijn indices,
--      have all functions defined at top-level,
--      have type annotations on every bound variable and constructor,
--      be a-normalised,
--      have saturated function applications,
--      not have over-applied function applications.
--      If not then `Error`.
--
--   The output code contains:
--      debruijn indices.
--       These then need to be eliminated before it will pass the Salt fragment
--       checks.
--
saltOfTetraModule
        :: Show a
        => Platform                             -- ^ Platform specification.
        -> A.Config                             -- ^ Runtime configuration.
        -> DataDefs E.Name                      -- ^ Data type definitions.
        -> KindEnv  E.Name                      -- ^ Kind environment.
        -> TypeEnv  E.Name                      -- ^ Type environment.
        -> Module (AnTEC a E.Name) E.Name       -- ^ Lite module to convert.
        -> Either (Error a) (Module a A.Name)   -- ^ Salt module.

saltOfTetraModule platform runConfig defs kenv tenv mm
 = {-# SCC saltOfTetraModule #-}
   evalCheck () $ convertM platform runConfig defs kenv tenv mm


---------------------------------------------------------------------------------------------------
convertM 
        :: Show a
        => Platform
        -> A.Config
        -> DataDefs E.Name
        -> KindEnv  E.Name
        -> TypeEnv  E.Name
        -> Module (AnTEC a E.Name) E.Name 
        -> ConvertM a (Module a A.Name)

convertM pp runConfig defs kenv tenv mm
  = do  
        -- All the data type definitions visible in the module.
        let defs'  = unionDataDefs defs
                   $ fromListDataDefs 
                   $ moduleDataDefsLocal mm ++ (map fst $ moduleImportDataDefs mm)

        -- Convert signatures of imported functions.
        tsImports' <- mapM (convertImportM defs') 
                   $ moduleImportValues mm

        -- Convert signatures of exported functions.
        tsExports' <- mapM (convertExportM defs') $ moduleExportValues mm


        -- Convert the body of the module to Salt.
        let ntsImports  
                   = [BName n (typeOfImportSource src) 
                        | (n, src) <- moduleImportValues mm]
        let tenv'  = Env.extends ntsImports tenv
        

        -- Top-level context for the conversion.
        let penv   = TopEnv
                   { topEnvPlatform     = pp
                   , topEnvDataDefs     = defs'
                   , topEnvSupers       = moduleTopBinds mm 
                   , topEnvImportValues = Set.fromList $ map fst $ moduleImportValues mm }

        -- Conver the body of the module itself.
        x1         <- convertExpX penv kenv tenv' ExpTop
                   $  moduleBody mm

        -- Converting the body will also expand out code to construct,
        -- the place-holder '()' inside the top-level lets.
        -- We don't want that, so just replace that code with a fresh unit.
        let a           = annotOfExp x1
        let (lts', _)   = splitXLets x1
        let x2          = xLets a lts' (xUnit a)

        -- Build the output module.
        let mm_salt 
                = ModuleCore
                { moduleName           = moduleName mm
                , moduleIsHeader       = moduleIsHeader mm

                  -- None of the types imported by Lite modules are relevant
                  -- to the Salt language.
                , moduleExportTypes    = []
                , moduleExportValues   = tsExports'

                , moduleImportTypes    = Map.toList $ A.runtimeImportKinds
                , moduleImportValues   = (Map.toList A.runtimeImportTypes) ++ tsImports'
                , moduleImportDataDefs = []

                  -- Data constructors and pattern matches should have been
                  -- flattenedinto primops, so we don't need the data type
                  -- definitions.
                , moduleDataDefsLocal  = []

                , moduleBody           = x2 }

        -- If this is the 'Main' module then add code to initialise the 
        -- runtime system. This will fail if given a Main module with no
        -- 'main' function.
        mm_init <- case initRuntime runConfig mm_salt of
                        Nothing   -> throw ErrorMainHasNoMain
                        Just mm'  -> return mm'

        return $ mm_init


---------------------------------------------------------------------------------------------------
-- | Convert an export spec.
convertExportM
        :: DataDefs E.Name
        -> (E.Name, ExportSource E.Name)                
        -> ConvertM a (A.Name, ExportSource A.Name)

convertExportM defs (n, esrc)
 = do   n'      <- convertBindNameM n
        esrc'   <- convertExportSourceM defs esrc
        return  (n', esrc')


-- Convert an export source.
convertExportSourceM 
        :: DataDefs E.Name
        -> ExportSource E.Name
        -> ConvertM a (ExportSource A.Name)

convertExportSourceM defs esrc
 = case esrc of
        ExportSourceLocal n t
         -> do  n'      <- convertBindNameM n
                t'      <- convertRepableT defs Env.empty t
                return  $ ExportSourceLocal n' t'

        ExportSourceLocalNoType n
         -> do  n'      <- convertBindNameM n
                return  $ ExportSourceLocalNoType n'


---------------------------------------------------------------------------------------------------
-- | Convert an import spec.
convertImportM
        :: DataDefs E.Name
        -> (E.Name, ImportSource E.Name)
        -> ConvertM a (A.Name, ImportSource A.Name)

convertImportM defs (n, isrc)
 = do   n'      <- convertImportNameM n
        isrc'   <- convertImportSourceM defs isrc
        return  (n', isrc')


-- | Convert an imported name.
--   These can be variable names for values, 
--   or variable or constructor names for type imports.
convertImportNameM :: E.Name -> ConvertM a A.Name
convertImportNameM n
 = case n of
        E.NameVar str   -> return $ A.NameVar str
        E.NameCon str   -> return $ A.NameCon str
        _               -> throw  $ ErrorInvalidBinder n


-- | Convert an import source.
convertImportSourceM 
        :: DataDefs E.Name
        -> ImportSource E.Name
        -> ConvertM a (ImportSource A.Name)

convertImportSourceM defs isrc
 = case isrc of
        ImportSourceAbstract t
         -> do  t'      <- convertRepableT defs Env.empty t
                return $ ImportSourceAbstract t'

        ImportSourceModule mn n t
         -> do  n'      <- convertBindNameM n
                t'      <- convertRepableT defs Env.empty t
                return  $ ImportSourceModule mn n' t'

        ImportSourceSea str t
         -> do  t'      <- convertRepableT defs Env.empty t 
                return  $ ImportSourceSea str t'


