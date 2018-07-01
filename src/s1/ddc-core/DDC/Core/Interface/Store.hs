
module DDC.Core.Interface.Store
        ( Store (..)
        , new, wrap, load
        , importValuesOfStore

        , Meta  (..)
        , getMeta
        , getModuleNames
        , getInterfaces

        , DataDef       (..)
        , ImportValue   (..)
        , findImportValue)
where
import DDC.Core.Interface.Error
import DDC.Core.Interface.Base
import DDC.Core.Module.Import
import DDC.Core.Module
import DDC.Type.DataDef
import DDC.Type.Exp
import DDC.Data.Pretty
import System.Directory
import Data.IORef
import Data.Maybe
import Data.Map                                 (Map)
import qualified Data.List                      as List
import qualified DDC.Core.Fragment.Profile      as C
import qualified DDC.Core.Codec.Shimmer.Decode  as Decode
import qualified SMR.Core.Exp                   as SMR
import qualified SMR.Prim.Name                  as SMR
import qualified Data.Map.Strict                as Map
import qualified Data.ByteString                as BS


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
        let mns    =  concatMap Map.toList $ Map.elems mnns

        putStrLn $ unlines $ map show mns

        return mns


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
        refMeta            <- newIORef []

        -- Name Indexes
        refTypeCtorNames   <- newIORef Map.empty
        refDataCtorNames   <- newIORef Map.empty
        refCapNames        <- newIORef Map.empty
        refValueNames      <- newIORef Map.empty

        -- Module Data
        refDataDefsByTyCon <- newIORef Map.empty
        refDataDefsByDaCon <- newIORef Map.empty
        refTypeDefsByTyCon <- newIORef Map.empty
        refCapsByName      <- newIORef Map.empty
        refValuesByName    <- newIORef Map.empty

        -- Complete Interfaces
        refInterfaces      <- newIORef []

        return  $ Store
                { storeMeta             = refMeta
                , storeTypeCtorNames    = refTypeCtorNames
                , storeDataCtorNames    = refDataCtorNames
                , storeCapNames         = refCapNames
                , storeValueNames       = refValueNames
                , storeDataDefsByTyCon  = refDataDefsByTyCon
                , storeDataDefsByDaCon  = refDataDefsByDaCon
                , storeTypeDefsByTyCon  = refTypeDefsByTyCon
                , storeCapsByName       = refCapsByName
                , storeValuesByName     = refValuesByName
                , storeInterfaces       = refInterfaces }


-- | Add information from a pre-loaded interface to the store.
wrap    :: (Ord n, Show n)
        => Store n -> Interface n -> IO ()
wrap store ii
 = do
        modifyIORef' (storeMeta store) $ \meta
         -> meta ++ [metaOfInterface ii]

        modifyIORef' (storeDataDefsByTyCon store) $ \ddefs
         -> Map.insert  (interfaceModuleName ii)
                        (dataDefsByTyConOfInterface ii)
                        ddefs

        modifyIORef' (storeValuesByName store) $ \ivs
         -> Map.unionWith Map.union ivs (importValuesOfInterface ii)

--        modifyIORef' (storeDataDefOfCtor store) $ \ctors
--         -> Map.union ctors (dataDefsOfCtorFromInterface ii)

        modifyIORef' (storeInterfaces store) $ \iis
         -> iis ++ [ii]


-- | Load a new interface from a file.
load    :: (Show n, Ord n)
        => C.Profile n
        -> (SMR.Exp Text SMR.Prim -> Maybe n)
        -> FilePath
        -> IO (Either Error (Interface n))

load profile takeRef filePath
 = do   let kenv   = C.profilePrimKinds profile
        let tenv   = C.profilePrimTypes profile
        let config = Decode.Config takeRef

        timeStamp  <- getModificationTime filePath

        -- Read the file source.
        bs      <- BS.readFile filePath

        let magicSMR    = BS.pack [0x53, 0x4d, 0x52, 0x31]

        if BS.isPrefixOf magicSMR (BS.take 4 bs)
         -- Binary interface file in Shimmer format.
         then do
                let iint = Decode.decodeInterface config kenv tenv filePath timeStamp bs
                case iint of
                 Just i  -> return $ Right i
                 Nothing -> return $ Left $ ErrorLoad filePath "unpack of Shimmer file failed"

         -- Textual interface file in Discus Source format.
         else do
                return $ Left $ ErrorBadMagic filePath 0



-- | Get metadata of interfaces currently in the store.
getMeta :: Store n -> IO [Meta]
getMeta store
 = do   mm      <- readIORef (storeMeta store)
        return  $ mm


-- | Get names of the modules currently in the store.
getModuleNames :: Store n -> IO [ModuleName]
getModuleNames store
 = do   metas   <- readIORef (storeMeta store)
        return  $ map metaModuleName metas


-- | Get the fully loaded interfaces.
getInterfaces :: Store n -> IO [Interface n]
getInterfaces store
 = do   ints    <- readIORef (storeInterfaces store)
        return ints


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


---------------------------------------------------------------------------------------------------
-- | Extract metadata from an interface.
metaOfInterface   :: Interface n -> Meta
metaOfInterface ii
        = Meta
        { metaFilePath   = interfaceFilePath   ii
        , metaTimeStamp  = interfaceTimeStamp  ii
        , metaModuleName = interfaceModuleName ii }


-- | Build nested maps from a list of triples.
--   Any values with duplicate 'a' and 'b' keys get overwritten by later ones.
nestMaps :: (Ord a, Ord b) => [(a, b, c)] -> Map a (Map b c)
nestMaps xs
 = List.foldl' insert Map.empty xs
 where  insert mpABC (a, b, c)
         = Map.alter (\entry
                -> case entry of
                        Nothing   -> Just (Map.singleton b c)
                        Just mpBC -> Just (Map.insert b c mpBC))
                a mpABC

-- Caps -------------------------------------------------------------------------------------------
{-
importCapsOfInterface
        :: Ord n => Interface
        -> Map ModuleName (Map n (ImportCap n (Type n)))
= let
        importOfExport
-}

-- Values -----------------------------------------------------------------------------------------
-- | Extract a map of super interfaces from the given module interface.
--
--   This contains all the information needed to import a super into
--   a client module.
--
importValuesOfInterface
        :: Ord n => Interface n
        -> Map ModuleName (Map n (ImportValue n (Type n)))
importValuesOfInterface ii
 = let
        -- These should only be present in the source language before,
        -- we've done type checking and resolved the type.
        importOfExport ExportValueLocalNoType{}
         = Nothing

        importOfExport ex@ExportValueLocal{}
         = Just ( exportValueLocalModuleName ex
                , exportValueLocalName ex
                , ImportValueModule
                  { importValueModuleName        = exportValueLocalModuleName ex
                  , importValueModuleVar         = exportValueLocalName ex
                  , importValueModuleType        = exportValueLocalType ex
                  , importValueModuleArity       = exportValueLocalArity ex })

        importOfExport ex@ExportValueSea{}
         = Just ( exportValueSeaModuleName ex
                , exportValueSeaNameInternal ex
                , ImportValueSea
                  { importValueSeaModuleName     = exportValueSeaModuleName ex
                  , importValueSeaNameInternal   = exportValueSeaNameInternal ex
                  , importValueSeaNameExternal   = exportValueSeaNameExternal ex
                  , importValueSeaType           = exportValueSeaType ex })

        importOfImport im@ImportValueModule{}
         = Just ( importValueModuleName im
                , importValueModuleVar im
                , im)

        importOfImport im@ImportValueSea{}
         = Just ( importValueSeaModuleName im
                , importValueSeaNameInternal im
                , im)

   in   Map.unionWith Map.union
         (nestMaps $ mapMaybe importOfExport
                   $ map snd $ moduleExportValues $ interfaceModule ii)
         (nestMaps $ mapMaybe importOfImport
                   $ map snd $ moduleImportValues $ interfaceModule ii)


-- | Extract a map of all data declarations by type constructor from a module interface.
dataDefsByTyConOfInterface :: Ord n => Interface n -> Map n (DataDef n)
dataDefsByTyConOfInterface ii
 = Map.union
        (Map.fromList [ (dataDefTypeName def, def)
                      | def <- map snd $ moduleImportDataDefs $ interfaceModule ii])
        (Map.fromList [ (dataDefTypeName def, def)
                      | def <- map snd $ moduleLocalDataDefs  $ interfaceModule ii])


-- | Take a module interface and extract a map of defined data constructor
--   names to their defining data type declaration. This includes data types
--   defined in the module itself, as well as imported ones.
{-
dataDefsOfCtorFromInterface :: Ord n => Interface n -> Map (ModuleName, n) (DataDef n)
dataDefsOfCtorFromInterface ii
 | Just mmDiscus <- interfaceModule ii
 = let
        takeCtorsOfDataDef dataDef
         = case dataDefCtors dataDef of
                Nothing    -> Map.empty
                Just ctors
                 -> Map.fromList
                        [ ((dataCtorModuleName ctor, dataCtorName ctor), dataDef)
                        | ctor <- ctors ]
   in   Map.unions
          [ Map.unions $ map takeCtorsOfDataDef $ map snd $ moduleImportDataDefs mmDiscus
          , Map.unions $ map takeCtorsOfDataDef $ map snd $ moduleLocalDataDefs  mmDiscus ]

 | otherwise
 = Map.empty
-}
