
module DDC.Build.Interface.Store
        ( Store (..)
        , new, wrap, load
        , importValuesOfStore

        , Meta  (..)
        , getMeta
        , getModuleNames
        , getInterfaces

        , Super (..)
        , findSuper)
where
import DDC.Build.Interface.Error
import DDC.Build.Interface.Base
import DDC.Core.Module
import DDC.Type.DataDef
import DDC.Type.Exp
import DDC.Data.Pretty
import System.Directory
import Data.Time.Clock
import Data.IORef
import Data.Maybe
import Data.Map                                                 (Map)
import qualified DDC.Build.Interface.Codec.Shimmer.Decode       as IS
import qualified DDC.Core.Discus                                as D
import qualified Data.Map.Strict                                as Map
import qualified Data.ByteString                                as BS


---------------------------------------------------------------------------------------------------
-- | Abstract API to a collection of module interfaces.
--
--   This lives in IO land because in future we want to demand-load the
--   inferface files as needed, rather than loading the full dependency
--   tree. Keeping it in IO means that callers must also be in IO.
data Store
        = Store
        { -- | Metadata for interface files currently in the store.
          storeMeta             :: IORef [Meta]

          -- | Lookup the definition of the given top-level super,
          --   from one or more of the provided modules.
        , storeSupers           :: IORef (Map ModuleName (Map D.Name Super))

          -- | Map of data type declarations.
        , storeDataDefs         :: IORef (Map (ModuleName, D.Name) (DataDef D.Name))

          -- | Map of data constructor name to the type declaration that introduces it.
        , storeDataDefOfCtor    :: IORef (Map (ModuleName, D.Name) (DataDef D.Name))

          -- | Fully loaded interface files.
          --   In future we want to load parts of interface files on demand,
          --   and not the whole lot.
        , storeInterfaces       :: IORef [InterfaceAA] }


-- | Get a list of types of all top-level supers in all modules in the store.
importValuesOfStore
        :: Store
        -> IO [(D.Name, ImportValue D.Name (Type D.Name))]

importValuesOfStore store
 = do   mnns            <- readIORef $ storeSupers store
        let mns         =  Map.elems mnns
        let supers      =  concatMap Map.elems mns
        return $ [ (superName s, superImportValue s)
                        | s <- supers ]


---------------------------------------------------------------------------------------------------
-- | Metadata for interfaces currently loaded into the store.
data Meta
        = Meta
        { metaFilePath     :: FilePath
        , metaTimeStamp    :: UTCTime
        , metaModuleName   :: ModuleName }
        deriving Show

instance Pretty Meta where
        ppr (Meta path stamp name)
         = hsep [ padL 60 $ string (show path)
                , padL 30 $ string (show stamp)
                , string (show name)]


-- | Interface for some top-level super.
data Super
        = Super
        { -- | Name of the super.
          superName             :: D.Name

          -- | Where the super was imported from.
          --
          --   This is the module that the name was resolved from. If that
          --   module re-exported an imported name then this may not be the
          --   module the super was actually defined in.
        , superModuleName       :: ModuleName

          -- | Tetra type for the super.
        , superTetraType        :: Type D.Name

          -- | Import source for the super.
          --
          --   This can be used to refer to the super from a client module.
        , superImportValue      :: ImportValue D.Name (Type D.Name) }


---------------------------------------------------------------------------------------------------
-- | An empty interface store.
new :: IO Store
new
 = do   refMeta          <- newIORef []
        refSupers        <- newIORef Map.empty
        refDataDefs      <- newIORef Map.empty
        refDataDefOfCtor <- newIORef Map.empty
        refInterfaces    <- newIORef []
        return  $ Store
                { storeMeta             = refMeta
                , storeSupers           = refSupers
                , storeDataDefs         = refDataDefs
                , storeDataDefOfCtor    = refDataDefOfCtor
                , storeInterfaces       = refInterfaces }


-- | Add a pre-loaded interface file to the store.
wrap    :: Store -> InterfaceAA -> IO ()
wrap store ii
 = do
        modifyIORef' (storeMeta store) $ \meta
         -> meta ++ [metaOfInterface ii]

        modifyIORef' (storeSupers store) $ \supers
         -> Map.insert (interfaceModuleName ii)
                       (supersFromInterface ii)
                       supers

        modifyIORef' (storeDataDefs store) $ \ddefs
         -> case interfaceDiscusModule ii of
             Nothing -> Map.empty
             Just mmDiscus
              -> Map.unions
                [ ddefs
                , Map.fromList
                        [ ((dataDefModuleName def, dataDefTypeName def), def)
                        | (_, def) <- moduleImportDataDefs mmDiscus ]
                , Map.fromList
                        [ ((dataDefModuleName def, dataDefTypeName def), def)
                        | (_, def) <- moduleLocalDataDefs  mmDiscus ]]

        modifyIORef' (storeDataDefOfCtor store) $ \ctors
         -> Map.union ctors (dataDefsOfCtorFromInterface ii)

        modifyIORef' (storeInterfaces store) $ \iis
         -> iis ++ [ii]


-- | Load a new interface from a file.
load    :: FilePath -> IO (Either Error InterfaceAA)
load filePath
 = do   timeStamp  <- getModificationTime filePath

        -- Read the file source.
        bs      <- BS.readFile filePath

        let magicSMR    = BS.pack [0x53, 0x4d, 0x52, 0x31]

        if BS.isPrefixOf magicSMR (BS.take 4 bs)
         -- Binary interface file in Shimmer format.
         then do
                let iint = IS.decodeInterface filePath timeStamp bs
                return iint

         -- Textual interface file in Discus Source format.
         else do
                return $ Left $ ErrorBadMagic filePath 0



-- | Get metadata of interfaces currently in the store.
getMeta :: Store -> IO [Meta]
getMeta store
 = do   mm      <- readIORef (storeMeta store)
        return  $ mm


-- | Get names of the modules currently in the store.
getModuleNames :: Store -> IO [ModuleName]
getModuleNames store
 = do   supers  <- readIORef (storeSupers store)
        return  $ Map.keys supers


-- | Get the fully loaded interfaces.
getInterfaces :: Store -> IO [InterfaceAA]
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
findSuper
        :: Store
        -> D.Name               -- ^ Name of desired super.
        -> [ModuleName]         -- ^ Names of modules to search.
        -> IO [Super]

findSuper store n modNames
 = do   supers  <- readIORef (storeSupers store)
        return $ mapMaybe
                (\modName -> do
                        nSupers <- Map.lookup modName supers
                        Map.lookup n nSupers)
                modNames


---------------------------------------------------------------------------------------------------
-- | Extract metadata from an interface.
metaOfInterface   :: InterfaceAA -> Meta
metaOfInterface ii
        = Meta
        { metaFilePath   = interfaceFilePath   ii
        , metaTimeStamp  = interfaceTimeStamp  ii
        , metaModuleName = interfaceModuleName ii }


-- | Extract a map of super interfaces from the given module interface.
--
--   This contains all the information needed to import a super into
--   a client module.
--
supersFromInterface :: InterfaceAA -> Map D.Name Super
supersFromInterface ii
 | Just mmDiscus <- interfaceDiscusModule ii
 = let
        -- The current module name.
        modName = interfaceModuleName ii

        -- Build a super declaration from an export.
        takeSuperOfExport ex@ExportValueLocal{}
         = Just $ Super
                { superName        = exportValueLocalName ex
                , superModuleName  = modName
                , superTetraType   = exportValueLocalType ex
                , superImportValue = ImportValueModule
                                   { importValueModuleName  = modName
                                   , importValueModuleVar   = exportValueLocalName ex
                                   , importValueModuleType  = exportValueLocalType ex
                                   , importValueModuleArity = exportValueLocalArity ex }
                }

        takeSuperOfExport ex@ExportValueSea{}
         = Just $ Super
                { superName        = exportValueSeaNameInternal ex
                , superModuleName  = modName
                , superTetraType   = exportValueSeaType ex
                , superImportValue = ImportValueSea
                                   { importValueSeaNameInternal = exportValueSeaNameInternal ex
                                   , importValueSeaNameExternal = exportValueSeaNameExternal ex
                                   , importValueSeaType         = exportValueSeaType ex }
                }

        takeSuperOfExport _
         = Nothing

   in   Map.fromList
                [ (n, let Just s = takeSuperOfExport ex in s)
                | (n, ex) <- moduleExportValues mmDiscus]

 | otherwise
 = Map.empty


-- | Take a module interface and extract a map of defined data constructor
--   names to their defining data type declaration. This includes data types
--   defined in the module itself, as well as imported ones.
dataDefsOfCtorFromInterface :: InterfaceAA -> Map (ModuleName, D.Name) (DataDef D.Name)
dataDefsOfCtorFromInterface ii
 | Just mmDiscus <- interfaceDiscusModule ii
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

