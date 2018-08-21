{-# OPTIONS_HADDOCK hide #-}

module DDC.Core.Interface.Store.Construct where
import DDC.Core.Interface.Store.Base
import DDC.Core.Module.Import
import DDC.Core.Module
import qualified DDC.Core.Exp.Annot     as C
import DDC.Type.DataDef
import DDC.Type.Exp
import Data.IORef
import Data.Maybe
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set


---------------------------------------------------------------------------------------------------
-- | An empty interface store.
new :: IO (Store n)
new
 = do   -- Interface metadata.
        refMeta                         <- newIORef Map.empty

        -- Name Indexes
        refTypeCtorNames                <- newIORef Map.empty
        refDataCtorNames                <- newIORef Map.empty
        refCapNames                     <- newIORef Map.empty
        refValueNames                   <- newIORef Map.empty
        refValueTyCons                  <- newIORef Map.empty

        -- Module Deps
        refModuleTransitiveDeps         <- newIORef Map.empty

        -- Module Exports
        refExportTypesOfModule          <- newIORef Map.empty
        refExportValuesOfModule         <- newIORef Map.empty

        -- Module Data
        refDataTypesByTyCon             <- newIORef Map.empty
        refTypeSynsByTyCon              <- newIORef Map.empty
        refForeignTypesByTyCon          <- newIORef Map.empty
        refDataCtorsByDaCon             <- newIORef Map.empty
        refCapsByName                   <- newIORef Map.empty
        refValuesByName                 <- newIORef Map.empty
        refValuesByResultTyCon          <- newIORef Map.empty

        -- Complete Interfaces
        refInterfaces                   <- newIORef Map.empty

        return
         $ Store
         { storeMeta                    = refMeta

         -- Name Indices
         , storeTypeCtorNames           = refTypeCtorNames
         , storeDataCtorNames           = refDataCtorNames
         , storeCapNames                = refCapNames
         , storeValueNames              = refValueNames
         , storeValueTyCons             = refValueTyCons

         -- Module Deps
         , storeModuleTransitiveDeps    = refModuleTransitiveDeps

         -- Module Exports
         , storeExportTypesOfModule     = refExportTypesOfModule
         , storeExportValuesOfModule    = refExportValuesOfModule

         -- Module Data
         , storeDataTypesByTyCon        = refDataTypesByTyCon
         , storeForeignTypesByTyCon     = refForeignTypesByTyCon
         , storeTypeSynsByTyCon         = refTypeSynsByTyCon
         , storeDataCtorsByDaCon        = refDataCtorsByDaCon
         , storeCapsByName              = refCapsByName
         , storeValuesByName            = refValuesByName
         , storeValuesByResultTyCon     = refValuesByResultTyCon
         , storeInterfaces              = refInterfaces

         -- Fetch Functions
         , storeLoadInterface           = Nothing }


---------------------------------------------------------------------------------------------------
-- | Add information from a pre-loaded interface to the store.
addInterface
        :: (Ord n, Show n)
        => Store n -> Interface n -> IO ()

addInterface store ii
 = do   let nModule     = interfaceModuleName ii
        let mm          = interfaceModule ii

        -- Add interface metadata.
        modifyIORef' (storeMeta store) $ \meta
         -> Map.insert  (interfaceModuleName ii)
                        (metaOfInterface ii)
                        meta

        -- Add whole interface to store.
        modifyIORef' (storeInterfaces store) $ \iis
         -> Map.insert nModule ii iis

        -- Add module transitive deps
        modifyIORef' (storeModuleTransitiveDeps store) $ \deps
         -> Map.insert nModule (moduleTransitiveDeps mm) deps

        -- Add data type definitions.
        let dataTypesByTyCon = dataTypesByTyConOfInterface ii
        modifyIORef' (storeDataTypesByTyCon store) $ \ddefs
         -> Map.insert nModule dataTypesByTyCon ddefs

        -- Add foreign type definitions.
        let foreignTypesByTyCon = foreignTypesByTyConOfInterface ii
        modifyIORef' (storeForeignTypesByTyCon store) $ \its
         -> Map.insert nModule foreignTypesByTyCon its

        -- Add type synonym definitions.
        let typeSynsByTyCon  = typeSynsByTyConOfInterface ii
        modifyIORef' (storeTypeSynsByTyCon store) $ \tdefs
         -> Map.insert nModule typeSynsByTyCon tdefs

        -- Add data ctor definitions.
        let dataCtorsByDaCon = dataCtorsByDaConOfInterface ii
        modifyIORef' (storeDataCtorsByDaCon store) $ \ddefs
         -> Map.insert nModule dataCtorsByDaCon ddefs

        -- Add value definitions.
        let importValues = importValuesOfInterface ii
        modifyIORef' (storeValuesByName store) $ \ivs
         -> Map.insert nModule importValues ivs

        let importValuesTC = slurpImportValuesByResultTyCon importValues
        modifyIORef' (storeValuesByResultTyCon store) $ \ivs
         -> Map.insert nModule importValuesTC ivs

        -- Update type ctor index.
        modifyIORef' (storeTypeCtorNames store) $ \names
         -> Map.unionsWith Set.union
                [ names
                , Map.fromList [ (n, Set.singleton nModule) | n <- Map.keys dataTypesByTyCon ]
                , Map.fromList [ (n, Set.singleton nModule) | n <- Map.keys typeSynsByTyCon  ]
                , Map.fromList [ (n, Set.singleton nModule) | n <- Map.keys foreignTypesByTyCon ] ]

        -- Update data ctor index.
        modifyIORef' (storeDataCtorNames store) $ \names
         -> Map.unionsWith Set.union
                [ names
                , Map.fromList  [ (n, Set.singleton nModule)
                                | n <- Map.keys dataCtorsByDaCon ] ]

        -- Update value names index.
        modifyIORef' (storeValueNames store) $ \names
         -> Map.unionsWith Set.union
                [ names
                , Map.fromList  [ (n, Set.singleton nModule)
                                | n <- Map.keys importValues ] ]

        -- Update value result tycons index.
        modifyIORef' (storeValueTyCons store) $ \names
         -> Map.unionsWith Set.union
                [ names
                , Map.fromList  [ (n, Set.singleton nModule)
                                | n <- Map.keys importValuesTC ]]

        return ()


---------------------------------------------------------------------------------------------------
-- | Extract a map of all data declarations by type constructor from a module interface.
dataTypesByTyConOfInterface :: Ord n => Interface n -> Map n (DataType n)
dataTypesByTyConOfInterface ii
        = dataDefsTypes
        $ fromListDataDefs $ Map.elems
        $ Map.union
                (Map.fromList [ (dataDefTypeName def, def)
                      | def <- map snd $ moduleImportDataDefs $ interfaceModule ii])
                (Map.fromList [ (dataDefTypeName def, def)
                      | def <- map snd $ moduleLocalDataDefs  $ interfaceModule ii])


-- | Extract a map of all foreign type declarations by type constructor from a module interfacce.
foreignTypesByTyConOfInterface :: Ord n => Interface n -> Map n (ImportType n (Kind n))
foreignTypesByTyConOfInterface ii
 = Map.fromList $ moduleImportTypes $ interfaceModule ii


-- | Extract a map of all type synonyms by type constructor from a module interface.
typeSynsByTyConOfInterface :: Ord n => Interface n -> Map n (Kind n, Type n)
typeSynsByTyConOfInterface ii
 = Map.union    (Map.fromList $ moduleImportTypeDefs $ interfaceModule ii)
                (Map.fromList $ moduleLocalTypeDefs  $ interfaceModule ii)


-- | Extract a map of all data ctor definitions by data constructor from a module interface.
dataCtorsByDaConOfInterface :: Ord n => Interface n -> Map n (DataCtor n, DataType n)
dataCtorsByDaConOfInterface ii
 = Map.fromList $ map make $ Map.elems dctors
 where  make dctor
         = let Just dtype = Map.lookup (dataCtorTypeName dctor) dtypes
           in  (dataCtorName dctor, (dctor, dtype))

        dtypes  = dataDefsTypes ddefs
        dctors  = dataDefsCtors ddefs
        ddefs   = fromListDataDefs
                $ Map.elems $ Map.union
                        (Map.fromList [ (dataDefTypeName def, def)
                              | def <- map snd $ moduleImportDataDefs $ interfaceModule ii])
                        (Map.fromList [ (dataDefTypeName def, def)
                              | def <- map snd $ moduleLocalDataDefs  $ interfaceModule ii])


-- Values -----------------------------------------------------------------------------------------
-- | Extract a map of super interfaces from the given module interface.
--
--   This contains all the information needed to import a super into
--   a client module.
--
--   TODO: don't auto expose imported values, only ones exported.
--
importValuesOfInterface :: Ord n => Interface n -> Map n (ImportValue n (Type n))
importValuesOfInterface ii
 = let
        -- These should only be present in the source language before,
        -- we've done type checking and resolved the type.
        importOfExport ExportValueLocalNoType{}
         = Nothing

        importOfExport ex@ExportValueLocal{}
         = Just ( exportValueLocalName ex
                , ImportValueModule
                  { importValueModuleName        = exportValueLocalModuleName ex
                  , importValueModuleVar         = exportValueLocalName ex
                  , importValueModuleType        = exportValueLocalType ex
                  , importValueModuleArity       = exportValueLocalArity ex })

        importOfExport ex@ExportValueSea{}
         = Just ( exportValueSeaNameInternal ex
                , ImportValueSea
                  { importValueSeaModuleName     = exportValueSeaModuleName ex
                  , importValueSeaNameInternal   = exportValueSeaNameInternal ex
                  , importValueSeaNameExternal   = exportValueSeaNameExternal ex
                  , importValueSeaType           = exportValueSeaType ex })

        importOfImport im@ImportValueModule{}
         = Just ( importValueModuleVar im
                , im)

        importOfImport im@ImportValueSea{}
         = Just ( importValueSeaNameInternal im
                , im)

   in   Map.union
         (Map.fromList  $ mapMaybe importOfExport
                        $ map snd $ moduleExportValues $ interfaceModule ii)
         (Map.fromList  $ mapMaybe importOfImport
                        $ map snd $ moduleImportValues $ interfaceModule ii)


-- | Extract a map of ImportValues by the tycon of the result value,
--   from the map produced by the above.
--
--   This map is used when resolving elaborations. The resolve process needs
--   to find all the ways that it can build a value of a particular result type.
slurpImportValuesByResultTyCon
        :: Ord n
        => Map n (ImportValue n (Type n))
        -> Map n [ImportValue n (Type n)]

slurpImportValuesByResultTyCon mp
 = Map.unionsWith (++)
 [ Map.singleton n [iv]
        |  Just (n, iv)
        <- map slurpResolvable $ Map.elems mp ]
 where
        slurpResolvable iv
         = let  -- Get the full type of the value.
                tImport   = typeOfImportValue iv

                -- Split off the quantifiers to get just the body.
                tBody     = case C.takeTForalls tImport of
                                Nothing         -> tImport
                                Just (_bs, t)   -> t

                -- Split off the implicit parameters that can be resolved
                -- by the elaborator.
                tExplicit = snd $ C.takeTFunImplicits tBody

           in   case C.takeTyConApps tExplicit of
                 Just (TyConBound n, _) -> Just (n, iv)
                 _                      -> Nothing

