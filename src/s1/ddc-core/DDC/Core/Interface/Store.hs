
module DDC.Core.Interface.Store
        ( Store (..)
        , Meta  (..)
        , getMeta
        , getModuleNames
        , getInterfaces

        , new
        , addInterface
        , ensureInterface
        , importValuesOfStore
        , typeSynsOfStore

        , DataDef       (..)
        , ImportValue   (..)
        , findImportValue)
where
import DDC.Core.Interface.Base
import DDC.Core.Module.Import
import DDC.Core.Module
import DDC.Type.DataDef
import DDC.Type.Exp
import DDC.Data.Pretty
import Data.IORef
import Data.Maybe
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set


---------------------------------------------------------------------------------------------------
instance Pretty Meta where
        ppr (Meta path stamp name)
         = hsep [ padL 60 $ string (show path)
                , padL 30 $ string (show stamp)
                , string (show name)]


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

        -- Complete Interfaces
        refInterfaces                   <- newIORef []

        return
         $ Store
         { storeMeta                    = refMeta

         -- Name Indices
         , storeTypeCtorNames           = refTypeCtorNames
         , storeDataCtorNames           = refDataCtorNames
         , storeCapNames                = refCapNames
         , storeValueNames              = refValueNames

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
         , storeInterfaces              = refInterfaces

         -- Fetch Functions
         , storeLoadInterface           = Nothing }


---------------------------------------------------------------------------------------------------
-- | Get metadata of interfaces currently in the store.
getMeta :: Store n -> IO [Meta]
getMeta store
 = do   mm      <- readIORef (storeMeta store)
        return  $ Map.elems mm


-- | Get names of the modules currently in the store.
getModuleNames :: Store n -> IO [ModuleName]
getModuleNames store
 = do   metas   <- readIORef (storeMeta store)
        return  $ Map.keys metas


-- | Get the fully loaded interfaces.
getInterfaces :: Store n -> IO [Interface n]
getInterfaces store
 = do   ints    <- readIORef (storeInterfaces store)
        return ints


---------------------------------------------------------------------------------------------------
-- | Try to find and load the interface file for the given module into the store,
--   or do nothing if we already have it.

--   If the interface file cannot be found then return False, otherwise True.
--   If the interface file exists but cannot be loaded then `error`.
--   If there is no load function defined then `error`.
--
--   FIXME: we need to check that the interface file is fresh relative
--          to any existing source files and dependent modules. When statting the dep
--          modules also make sure to avoid restatting the same module over and over.
--          The top level compile driver used to do this job.
--
ensureInterface
        :: (Ord n, Show n)
        => Store n -> ModuleName -> IO Bool

ensureInterface store nModule
 | Just load   <- storeLoadInterface store
 = let
        goCheckMeta
         = do   meta <- readIORef $ storeMeta store
                case Map.lookup nModule meta of
                 Just _  -> return True
                 Nothing -> goLoad

        goLoad
         = do   result <- load nModule
                case result of
                 Nothing -> return False
                 Just ii
                  -> do addInterface store ii
                        return True
   in   goCheckMeta

 | otherwise
 = return False


---------------------------------------------------------------------------------------------------
-- | Add information from a pre-loaded interface to the store.
addInterface
        :: (Ord n, Show n)
        => Store n -> Interface n -> IO ()

addInterface store ii
 = do   let nModule  = interfaceModuleName ii

        -- Add interface metadata.
        modifyIORef' (storeMeta store) $ \meta
         -> Map.insert  (interfaceModuleName ii)
                        (metaOfInterface ii)
                        meta

        -- Add whole interface to store.
        modifyIORef' (storeInterfaces store) $ \iis
         -> iis ++ [ii]

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

        return ()


---------------------------------------------------------------------------------------------------
-- | See if a super is defined in any of the given modules, and if so
--   return the module name and super type.
--
--   NOTE: This function returns an IO [Super] in preparation for the case
--   where we load data from interface files on demand. We want to ensure
--   that the caller is also in IO, to make the refactoring easier later.
--
findImportValue
        :: Ord n
        => Store n
        -> n            -- ^ Name of desired super.
        -> [ModuleName] -- ^ Names of modules to search.
        -> IO [ImportValue n (Type n)]

findImportValue store n modNames
 = do   mivs  <- readIORef (storeValuesByName store)
        return $ mapMaybe
                (\modName -> do
                        ivs <- Map.lookup modName mivs
                        Map.lookup n ivs)
                modNames


-- | Build nested maps from a list of triples.
--   Any values with duplicate 'a' and 'b' keys get overwritten by later ones.
{-
nestMaps :: (Ord a, Ord b) => [(a, b, c)] -> Map a (Map b c)
nestMaps xs
 = List.foldl' insert Map.empty xs
 where  insert mpABC (a, b, c)
         = Map.alter (\entry
                -> case entry of
                        Nothing   -> Just (Map.singleton b c)
                        Just mpBC -> Just (Map.insert b c mpBC))
                a mpABC
-}

---------------------------------------------------------------------------------------------------
-- | Extract metadata from an interface.
metaOfInterface   :: Interface n -> Meta
metaOfInterface ii
        = Meta
        { metaFilePath   = interfaceFilePath   ii
        , metaTimeStamp  = interfaceTimeStamp  ii
        , metaModuleName = interfaceModuleName ii }



-- Caps -------------------------------------------------------------------------------------------
{-
importCapsOfInterface
        :: Ord n => Interface
        -> Map ModuleName (Map n (ImportCap n (Type n)))
= let
        importOfExport
-}

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


---------------------------------------------------------------------------------------------------
-- | Get a list of types of all top-level supers in all modules in the store.
--
--   TODO: multiple conflicting names get smashed together,
--   this is used by the elaborate pass which needs to be redone to use
--   the store directly.
--
importValuesOfStore :: (Ord n, Show n) => Store n -> IO [(n, ImportValue n (Type n))]
importValuesOfStore store
 = do   mnns       <- readIORef $ storeValuesByName store
        let mns    =  Map.toList $ Map.unions $ Map.elems mnns
        return mns


typeSynsOfStore :: (Ord n, Show n) => Store n -> IO [(n, Type n)]
typeSynsOfStore store
 = do   mnns       <- readIORef $ storeTypeSynsByTyCon store
        let mns    =  Map.toList $ Map.map snd $ Map.unions $ Map.elems mnns
        return mns

