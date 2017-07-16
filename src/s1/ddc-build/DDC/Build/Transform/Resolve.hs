
-- | Gather up the set of free names and try to find matching export statements
--   in the imported modules. Add explicit imports that refer to the module where
--   each name is bound.
module DDC.Build.Transform.Resolve
        (resolveNamesInModule )
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Collect.Support
import DDC.Type.DataDef
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Data.Pretty
import DDC.Control.Panic
import Data.Map                                 (Map)
import DDC.Build.Interface.Store                (Store)
import DDC.Build.Interface.Base                 (Interface (..))
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified DDC.Build.Interface.Store      as Store
import qualified DDC.Core.Tetra                 as E
import Data.List
import Data.Function
import qualified Data.Either                    as Either


-- | For all the names that are free in this module, if there is a
--   corresponding export in one of the modules in the given map,
--   then add the appropriate import definition.
resolveNamesInModule
        :: KindEnv E.Name       -- ^ Kinds of primitive types.
        -> TypeEnv E.Name       -- ^ Types of primitive values.
        -> Store                -- ^ Interface store.
        -> Module a E.Name      -- ^ Module to resolve names in.
        -> IO (Module a E.Name, [Error])

resolveNamesInModule kenv tenv store mm
 = do
        let sp      = support kenv tenv mm
        ints    <- Store.getInterfaces store
        let deps    = Map.fromList
                        [ ( interfaceModuleName i
                          , let Just m = interfaceTetraModule i in m)
                          | i <- ints ]
        modNames       <- Store.getModuleNames store

        let getDaVarImport (UName n) = do
                eImport <- findImportSourceForDaVar store modNames n
                case eImport of
                 Left err       -> return $ Left err
                 Right isrc     -> return $ Right (n, isrc)

            getDaVarImport u    =   panic "ddc-build" "resolveNamesInModule"
                                $   text  "Cannot resolve anonymous binder:"
                                <+> ppr u

        (errsDaVar, importsDaVar)
                <- fmap Either.partitionEithers
                $  mapM getDaVarImport
                $  Set.toList $ supportDaVar sp

        let mm_resolved
                = mm
                { moduleImportTypes
                        =  moduleImportTypes  mm
                        ++ importsForTyCons   deps (Set.toList $ supportTyCon sp)

                , moduleImportDataDefs
                     =  nubBy ((==) `on` dataDefTypeName)
                     $  moduleImportDataDefs mm
                     ++ importsForDaTyCons deps (Set.toList $ supportTyCon sp)

                , moduleImportTypeDefs
                     =  nubBy ((==) `on` fst)
                     $  moduleImportTypeDefs mm
                     ++ importsTypeDef deps

                , moduleImportCaps
                     =  moduleImportCaps mm
                     ++ importsCap deps

                , moduleImportValues
                     =  moduleImportValues mm
                     ++ importsDaVar }

        return (mm_resolved, errsDaVar)


---------------------------------------------------------------------------------------------------
-- | Import tycons defined in other modules.
importsForTyCons
        :: Ord n
        => Map ModuleName (Module b n)  -- ^ Modules which this one depends on.
        -> [Bound n]                    -- ^ Unbound type constructors to find imports for.
        -> [(n, ImportType n (Type n))]

importsForTyCons deps _tyCons
 = concat
        [ [(n, ImportTypeAbstract k)
                | (n, k)        <- Map.toList $ Map.unions
                                $  map importedTyConsAbs   $ Map.elems deps]

        , [(n, ImportTypeAbstract k)
                | (n, (_, k))   <- Map.toList $ Map.unions
                                $  map exportedTyConsLocal $ Map.elems deps]

        , [(n, ImportTypeBoxed k)
                | (n, k)        <- Map.toList $ Map.unions
                                $  map importedTyConsBoxed $ Map.elems deps] ]


---------------------------------------------------------------------------------------------------
-- | Import caps defined in other modules.
importsCap
        :: Map ModuleName (Module b n)
        -> [(n, ImportCap n (Type n))]

importsCap deps
 = concatMap moduleImportCaps $ Map.elems deps


---------------------------------------------------------------------------------------------------
-- | Import data defs defined in other modules.
importsForDaTyCons
        :: Map ModuleName (Module b n)
        -> [Bound n]
        -> [DataDef n]

importsForDaTyCons deps _tycons
        = concat
        $ [ moduleImportDataDefs m ++ moduleDataDefsLocal m
                | m <- Map.elems deps ]


---------------------------------------------------------------------------------------------------
-- | Import type defs defined in other modules.
importsTypeDef
        :: Map ModuleName (Module b n)
        -> [(n, (Kind n, Type n))]

importsTypeDef deps
        = concat
        $ [ moduleImportTypeDefs m ++ moduleTypeDefsLocal m
                | m <- Map.elems deps ]


---------------------------------------------------------------------------------------------------
-- | Build import statements for the given list of unbound value variables.
--
--   We look in dependency modules for a matching export,
--   and produce the corresponding import statement to use it.
--
findImportSourceForDaVar
        :: Store                -- ^ Interface store.
        -> [ModuleName]         -- ^ Modules to search for matching exports.
        -> E.Name               -- ^ Name of value.
        -> IO (Either Error (ImportValue E.Name (Type E.Name)))

findImportSourceForDaVar store modNames nSuper
 = do   result  <- Store.findSuper store nSuper modNames
        case result of
         []      -> return $ Left  $ ErrorNotFound nSuper
         [super] -> return $ Right $ Store.superImportValue super
         supers  -> return $ Left  $ ErrorMultiple nSuper (map Store.superModuleName supers)


---------------------------------------------------------------------------------------------------
-- | Get the tycons that are locally defined, then exported by a module.
exportedTyConsLocal :: Ord n => Module b n -> Map n (ModuleName, Kind n)
exportedTyConsLocal mm
        = Map.fromList
        $ [ (n, (moduleName mm, t))
                        | (n, ExportSourceLocal _ t) <- moduleExportTypes mm ]

-- | Get the type constructors that are imported abstractly by a module.
importedTyConsAbs  :: Ord n => Module b n -> Map n (Kind n)
importedTyConsAbs mm
        = Map.fromList
        $ [ (n, k)      | (n, ImportTypeAbstract k)  <- moduleImportTypes mm ]


-- | Get the type constructors that are imported as boxed foreign types.
importedTyConsBoxed :: Ord n => Module b n -> Map n (Kind n)
importedTyConsBoxed mm
        = Map.fromList
        $ [ (n, k)      | (n, ImportTypeBoxed k)      <- moduleImportTypes mm ]


---------------------------------------------------------------------------------------------------
-- | Problems that can be uncovered when resolving names.
data Error
        = ErrorNotFound E.Name
        | ErrorMultiple E.Name [ModuleName]

instance Pretty Error where
 ppr err
  = case err of
        ErrorNotFound n
         -> vcat [ text "Not in scope: " <> squotes (ppr n) ]

        ErrorMultiple n ms
         -> vcat $  [ text "Variable" <+> squotes (ppr n) <+> text "defined in multiple modules:" ]
                 ++ (map ppr ms)

