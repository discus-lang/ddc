
-- | Conversion of Disciple Core Discus to Disciple Core Salt.
module DDC.Core.Discus.Convert
        ( saltOfDiscusModule
        , Error(..))
where
import DDC.Core.Discus.Transform.Curry.Callable
import DDC.Core.Discus.Convert.Exp.Lets
import DDC.Core.Discus.Convert.Exp.Alt
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Exp
import DDC.Core.Discus.Convert.Type
import DDC.Core.Discus.Convert.Error
import qualified DDC.Core.Discus.Convert.Type.Base       as T

import DDC.Core.Salt.Convert                            (initRuntime)
import DDC.Core.Salt.Platform
import DDC.Core.Exp.Annot
import DDC.Core.Module
import DDC.Core.Call
import DDC.Core.Check                                   (AnTEC(..))
import qualified DDC.Core.Discus.Prim                    as E
import qualified DDC.Core.Salt.Runtime                  as A
import qualified DDC.Core.Salt.Name                     as A

import DDC.Type.DataDef
import DDC.Type.Env                                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env                           as Env

import DDC.Control.Check                                (throw, evalCheck)
import Data.Map                                         (Map)
import qualified Data.Map.Strict                        as Map
import qualified Data.Set                               as Set


---------------------------------------------------------------------------------------------------
-- | Convert a Core Discus module to Core Salt.
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
saltOfDiscusModule
        :: Show a
        => Platform                             -- ^ Platform specification.
        -> A.Config                             -- ^ Runtime configuration.
        -> DataDefs E.Name                      -- ^ Data type definitions.
        -> KindEnv  E.Name                      -- ^ Kind environment.
        -> TypeEnv  E.Name                      -- ^ Type environment.
        -> Module (AnTEC a E.Name) E.Name       -- ^ Discus module to convert.
        -> Either (Error a) (Module a A.Name)   -- ^ Salt module.

saltOfDiscusModule platform runConfig defs kenv tenv mm
 = {-# SCC saltOfDiscusModule #-}
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
                   $ fromListDataDefs $ map snd
                   $ moduleImportDataDefs mm ++ moduleLocalDataDefs mm

        let eqns'  = Map.unions
                   [ Map.fromList $ [(n, t) | (n, (_, t)) <- moduleImportTypeDefs mm]
                   , Map.fromList $ [(n, t) | (n, (_, t)) <- moduleLocalTypeDefs  mm] ]

        let nsForeignBoxedTypes
                   = [n | (n, ImportTypeBoxed _) <- moduleImportTypes mm ]

        let tctx'  = T.Context
                   { T.contextDataDefs  = defs'
                   , T.contextTypeEqns  = eqns'
                   , T.contextForeignBoxedTypeCtors
                        = Set.fromList nsForeignBoxedTypes
                   , T.contextKindEnv   = Env.empty }

        -- Module body ------------------------------------
        let ntsImports
                   = [BName n (typeOfImportValue src)
                     | (n, src) <- moduleImportValues mm]

        let tenv'  = Env.extends ntsImports tenv

        -- Get the call patterns of the callable things
        -- defined in this module.
        callables  <- either (throw . ErrorCurry) return
                   $  takeCallablesOfModule mm

        -- Starting context for the conversion.
        let ctx = Context
                { contextPlatform       = pp
                , contextDataDefs       = defs'
                , contextTypeEqns       = eqns'
                , contextForeignBoxedTypeCtors
                                        = Set.fromList $ nsForeignBoxedTypes
                , contextCallable       = callables
                , contextKindEnv        = kenv
                , contextTypeEnv        = tenv'
                , contextSuperBinds     = Map.empty
                , contextConvertExp     = convertExp
                , contextConvertLets    = convertLets
                , contextConvertAlt     = convertAlt }

        -- Convert the body of the module itself.
        x1      <- convertExp ExpTop ctx
                $  moduleBody mm

        -- Running the Discus -> Salt converted on the module body will also
        -- expand out code to construct the place holder expression '()'
        -- that is the body of the top-level letrec. We don't want that,
        -- so just replace it with a fresh unit.
        let a           = annotOfExp x1
        let (lts', _)   = splitXLets x1
        let x2          = xLets a lts' (xUnit a)

        -- Imports and Exports ----------------------------
        -- Convert signatures of imported functions.
        ntsImports'     <- mapM (convertNameImportValueM tctx')
                        $  moduleImportValues mm

        -- Convert signatures of exported functions.
        --  Locally defined values can be exported,
        --  and imported values can be re-exported.
        let ntsImport'  =  [(n, typeOfImportValue iv) | (n, iv) <- ntsImports']
        let ntsSuper'   =  [(n, t) | BName n t <- concat $ map snd $ map bindsOfLets lts']
        let ntsAvail    =  Map.fromList $ ntsSuper' ++ ntsImport'

        ntsExports'     <- mapM (convertNameExportValueM tctx' ntsAvail)
                        $  moduleExportValues mm

        -- Build the output module.
        let mm_salt
                = ModuleCore
                { moduleName            = moduleName mm
                , moduleIsHeader        = moduleIsHeader mm

                  -- None of the types imported by Discus modules are relevant
                  -- to the Salt language.
                , moduleExportTypes     = []
                , moduleExportValues    = ntsExports'

                , moduleImportTypes     = Map.toList $ A.runtimeImportKinds
                , moduleImportCaps      = []
                , moduleImportValues    = (Map.toList A.runtimeImportTypes) ++ ntsImports'
                , moduleImportDataDefs  = []
                , moduleImportTypeDefs  = []

                  -- Data constructors and pattern matches should have been
                  -- flattened into primops, so we don't need the data type
                  -- definitions.
                , moduleLocalDataDefs   = []
                , moduleLocalTypeDefs   = []

                , moduleBody            = x2 }

        -- If this is the 'Main' module then add code to initialise the
        -- runtime system. This will fail if given a Main module with no
        -- 'main' function.
        mm_init <- case initRuntime runConfig mm_salt of
                        Nothing   -> throw ErrorMainHasNoMain
                        Just mm'  -> return mm'

        return $ mm_init


---------------------------------------------------------------------------------------------------
-- | Convert an export spec.
convertNameExportValueM
        :: T.Context                     -- ^ Context of the conversion.
        -> Map A.Name (Type A.Name)      -- ^ Salt types of top-level values.
        -> (E.Name, ExportValue E.Name (Type E.Name))
                                        -- ^ Name and export def to convert.
        -> ConvertM a (A.Name, ExportValue A.Name (Type A.Name))

convertNameExportValueM tctx tsSalt (n, esrc)
 = do   n'      <- convertBindNameM n
        esrc'   <- convertExportValueM tctx tsSalt esrc
        return  (n', esrc')


-- Convert an export source.
--
--  We can't just convert the Discus type of an exported thing to the
--  corresponding Salt type as the form of the Salt type depends on
--  the arity of the underlying value. Instead, we lookup the Salt type
--  of each export from the list of previously known Salt types.
--
convertExportValueM
        :: T.Context                            -- ^ Context of the conversion.
        -> Map A.Name (Type A.Name)             -- ^ Salt types of top-level values.
        -> ExportValue E.Name (Type E.Name)     -- ^ Export source to convert.
        -> ConvertM a (ExportValue A.Name (Type A.Name))

convertExportValueM tctx tsSalt esrc
 = case esrc of
        ExportValueLocal n t _
         -> do  n'      <- convertBindNameM n

                case Map.lookup n' tsSalt of
                 -- We have a Salt type for this exported value.
                 Just t' -> return $ ExportValueLocal n' t' Nothing

                 -- If a type has been foreign imported from Salt land
                 -- then it won't be in the map, and we can just convert
                 -- its Discus type to get the Salt version.
                 Nothing
                  -> do t'      <- convertSuperT tctx t
                        return $ ExportValueLocal n' t' Nothing

        ExportValueLocalNoType n
         -> do  n'      <- convertBindNameM n
                return  $ ExportValueLocalNoType n'

        ExportValueSea n x t
         -> do  n'      <- convertBindNameM n
                t'      <- convertSuperT tctx t
                return  $ ExportValueSea n' x t'


---------------------------------------------------------------------------------------------------
-- | Convert an import spec.
convertNameImportValueM
        :: T.Context -> (E.Name, ImportValue E.Name (Type E.Name))
        -> ConvertM a (A.Name, ImportValue A.Name (Type A.Name))

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


-- | Convert an import source to Salt.
convertImportValueM
        :: T.Context -> ImportValue E.Name (Type E.Name)
        -> ConvertM a (ImportValue A.Name (Type A.Name))

convertImportValueM tctx isrc
 = case isrc of
        -- We have no arity information for some reason.
        --   Just convert the type assuming it's a standard call.
        --   If this is wrong then the Salt type checker will
        --   catch the problem.
        ImportValueModule mn n t Nothing
         -> do  let cs  =  takeCallConsFromType t
                n'      <- convertBindNameM n
                t'      <- convertSuperConsT tctx cs t
                return  $  ImportValueModule mn n' t' Nothing

        -- We have arity information for this thing from
        -- from the imported interface file.
        ImportValueModule mn n t (Just (nType, nValue, nBox))
         -> do  let Just cs = takeStdCallConsFromTypeArity t nType nValue nBox
                n'      <- convertBindNameM n
                t'      <- convertSuperConsT tctx cs t
                return  $  ImportValueModule mn n' t' Nothing

        -- We convert the types of Sea things directly.
        --   We assume that they don't return thunks,
        --   so we don't need any extra arity information to produce
        --   the Salt level type.
        ImportValueSea n str t
         -> do  n'      <- convertBindNameM n
                t'      <- convertSuperT tctx t
                return  $  ImportValueSea n' str t'

