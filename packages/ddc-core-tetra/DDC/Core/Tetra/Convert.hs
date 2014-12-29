
-- | Conversion of Disciple Core Tetra to Disciple Core Salt.
module DDC.Core.Tetra.Convert
        ( saltOfTetraModule
        , Error(..))
where
import DDC.Core.Tetra.Convert.Exp.Lets
import DDC.Core.Tetra.Convert.Exp.Alt
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Exp
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import qualified DDC.Core.Tetra.Convert.Type.Base       as T

import DDC.Core.Salt.Convert                            (initRuntime)
import DDC.Core.Salt.Platform
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                                   (AnTEC(..))
import qualified DDC.Core.Tetra.Prim                    as E
import qualified DDC.Core.Salt.Runtime                  as A
import qualified DDC.Core.Salt.Name                     as A

import DDC.Type.DataDef
import DDC.Type.Env                                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env                           as Env

import DDC.Control.Monad.Check                          (throw, evalCheck)
import Data.Maybe
import Control.Monad
import Data.Map                                         (Map)
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set


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
--      not have over-applied function applications,
--      have all supers in prenex form, with type parameters before value parameters.
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
        -> Module (AnTEC a E.Name) E.Name       -- ^ Tetra module to convert.
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
        -- Data Type definitions --------------------------
        -- All the data type definitions visible in the module.
        let defs'  = unionDataDefs defs
                   $ fromListDataDefs 
                   $ moduleImportDataDefs mm ++ moduleDataDefsLocal mm

        let nsForeignBoxedTypes
                   = [n | (n, ImportTypeBoxed _) <- moduleImportTypes mm ]

        let tctx'  = T.Context
                   { T.contextDataDefs  = defs'
                   , T.contextForeignBoxedTypeCtors     
                        = Set.fromList nsForeignBoxedTypes
                   , T.contextKindEnv   = Env.empty }

        -- Imports and Exports ----------------------------
        -- Convert signatures of imported functions.
        tsImports' <- mapM (convertNameImportValueM tctx') 
                   $ moduleImportValues mm

        -- Convert signatures of exported functions.
        tsExports' <- mapM (convertExportM tctx') 
                   $ moduleExportValues mm

        -- Module body ------------------------------------
        let ntsImports  
                   = [BName n (typeOfImportValue src) 
                     | (n, src) <- moduleImportValues mm]

        let tenv'  = Env.extends ntsImports tenv

        -- Check that all the supers are fully named, and are in prenex form.
        -- Also build a map of super names to their type and value arities.
        aritiesSuper  <- takePrenexAritiesOfTopBinds mm
        aritiesImport <- takePrenexAritiesOfImports  mm

        -- Starting context for the conversion.
        let ctx    = Context
                   { contextPlatform    = pp
                   , contextDataDefs    = defs'
                   , contextForeignBoxedTypeCtors = Set.fromList $ nsForeignBoxedTypes
                   , contextSupers      = aritiesSuper
                   , contextImports     = aritiesImport
                   , contextKindEnv     = kenv
                   , contextTypeEnv     = tenv' 
                   , contextSuperBinds  = Map.empty
                   , contextConvertExp  = convertExp 
                   , contextConvertLets = convertLets 
                   , contextConvertAlt  = convertAlt }

        -- Convert the body of the module itself.
        x1         <- convertExp ExpTop ctx 
                   $  moduleBody mm

        -- Running the Tetra -> Salt converted on the module body will also
        -- expand out code to construct the place holder expression '()' 
        -- that is the body of the top-level letrec. We don't want that,
        -- so just replace it with a fresh unit.
        let a           = annotOfExp x1
        let (lts', _)   = splitXLets x1
        let x2          = xLets a lts' (xUnit a)

        -- Build the output module.
        let mm_salt 
                = ModuleCore
                { moduleName           = moduleName mm
                , moduleIsHeader       = moduleIsHeader mm

                  -- None of the types imported by Tetra modules are relevant
                  -- to the Salt language.
                , moduleExportTypes    = []
                , moduleExportValues   = tsExports'

                , moduleImportTypes    = Map.toList $ A.runtimeImportKinds
                , moduleImportValues   = (Map.toList A.runtimeImportTypes) ++ tsImports'
                , moduleImportDataDefs = []

                  -- Data constructors and pattern matches should have been
                  -- flattened into primops, so we don't need the data type
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
        :: T.Context
        -> (E.Name, ExportSource E.Name)                
        -> ConvertM a (A.Name, ExportSource A.Name)

convertExportM tctx (n, esrc)
 = do   n'      <- convertBindNameM n
        esrc'   <- convertExportSourceM tctx esrc
        return  (n', esrc')


-- Convert an export source.
convertExportSourceM 
        :: T.Context
        -> ExportSource E.Name
        -> ConvertM a (ExportSource A.Name)

convertExportSourceM tctx esrc
 = case esrc of
        ExportSourceLocal n t
         -> do  n'      <- convertBindNameM n
                t'      <- convertSuperT  tctx t
                return  $ ExportSourceLocal n' t'

        ExportSourceLocalNoType n
         -> do  n'      <- convertBindNameM n
                return  $ ExportSourceLocalNoType n'


---------------------------------------------------------------------------------------------------
-- | Convert an import spec.
convertNameImportValueM
        :: T.Context -> (E.Name, ImportValue E.Name)
        -> ConvertM a (A.Name, ImportValue A.Name)

convertNameImportValueM tctx (n, isrc)
 = do   n'      <- convertImportNameM n
        isrc'   <- convertImportValueM tctx isrc
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
convertImportValueM 
        :: T.Context -> ImportValue E.Name
        -> ConvertM a (ImportValue A.Name)

convertImportValueM tctx isrc
 = case isrc of
        ImportValueModule mn n t _
         -> do  n'      <- convertBindNameM n
                t'      <- convertSuperT tctx t
                return  $ ImportValueModule mn n' t' Nothing

        ImportValueSea str t
         -> do  t'      <- convertSuperT tctx t 
                return  $ ImportValueSea str t'


---------------------------------------------------------------------------------------------------
-- | Check that all the supers in this module have real names, 
--   and are in prenex form -- meaning that they bind all their type parameters
--   before their value parameters.
--
--   If we find any supers where this is not the case then throw an error in 
--   the `ConvertM` monad.
--
takePrenexAritiesOfTopBinds 
        :: Module a E.Name -> ConvertM b (Map E.Name (Int, Int))

takePrenexAritiesOfTopBinds mm
 = do   
        let check (BName n _) (Just (ks, ts)) 
                = return (n, (length ks, length ts))

            check b Nothing     = throw $ ErrorSuperNotPrenex b
            check b  _          = throw $ ErrorSuperUnnamed   b
            
        nsArities       <- mapM (uncurry check)
                        $  mapTopBinds (\b x -> (b, takePrenexCallPattern x)) mm

        return $ Map.fromList nsArities


-- | Check that all the imported supers in the module have real names,
--   and are in prenex form.
--
takePrenexAritiesOfImports
        :: Module a E.Name -> ConvertM b (Map E.Name (Maybe (Int, Int)))

takePrenexAritiesOfImports mm
 = do   
        let check n (ImportValueModule _ _ _ (Just (aType, aValue)))
                = return $ Just (n, Just (aType, aValue))

            check n (ImportValueSea _ _)
                = return $ Just (n, Nothing)

            check _ _
                = return $ Nothing

        nsArities       <- liftM catMaybes
                        $  mapM (uncurry check)
                        $  moduleImportValues mm

        return $ Map.fromList nsArities

