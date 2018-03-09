
module DDC.Build.Interface.Store
        ( Store
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
import DDC.Type.Exp
import DDC.Data.Pretty
import System.Directory
import Data.Time.Clock
import Data.IORef
import Data.Maybe
import Data.Map                                                 (Map)
import qualified DDC.Build.Interface.Codec.Text.Decode          as IT
import qualified DDC.Build.Interface.Codec.Shimmer.Decode       as IS
import qualified DDC.Core.Discus                                as D
import qualified Data.Map.Strict                                as Map
import qualified Data.Compact                                   as Compact
import qualified Data.ByteString                                as BS
import qualified Data.Text.Encoding                             as Text
import qualified Data.Text                                      as Text

---------------------------------------------------------------------------------------------------
-- | Abstract API to a collection of module interfaces.
--
--   This lives in IO land because in future we want to demand-load the
--   inferface files as needed, rather than loading the full dependency
--   tree. Keeping it in IO means that callers must also be in IO.
data Store
        = Store
        { -- | Metadata for interface files currently in the store.
          storeMeta       :: IORef [Meta]

          -- | Lookup the definition of the given top-level super,
          --   from one or more of the provided modules.
        , storeSupers     :: IORef (Map ModuleName (Map D.Name Super))

          -- | Fully loaded interface files.
          --   In future we want to load parts of interface files on demand,
          --   and not the whole lot.
        , storeInterfaces :: IORef [InterfaceAA] }


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
         = hsep [ padL 60 $ text (show path)
                , padL 30 $ text (show stamp)
                , text (show name)]


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
 = do   refMeta         <- newIORef []
        refSupers       <- newIORef Map.empty
        refInterfaces   <- newIORef []
        return  $ Store
                { storeMeta             = refMeta
                , storeSupers           = refSupers
                , storeInterfaces       = refInterfaces }


-- | Add a pre-loaded interface file to the store.
wrap    :: Store -> InterfaceAA -> IO ()
wrap store ii
 = do   modifyIORef (storeMeta store)
         $ \meta   -> meta ++ [metaOfInterface ii]

        modifyIORef (storeSupers store)
         $ \supers -> Map.insert (interfaceModuleName ii)
                                 (supersOfInterface   ii)
                                 supers

        modifyIORef (storeInterfaces store)
         $ \iis    -> iis ++ [ii]


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
                cInt    <- Compact.compactWithSharing iint
                return  $  Compact.getCompact cInt

         -- Textual interface file in Discus Source format.
         else do
                let tx   = Text.decodeUtf8 bs
                let str  = Text.unpack tx
                let iint = IT.decodeInterface filePath timeStamp str
                cInt    <- Compact.compactWithSharing iint
                return  $  Compact.getCompact cInt


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
supersOfInterface :: InterfaceAA -> Map D.Name Super
supersOfInterface ii
 | Just mmDiscus <- interfaceDiscusModule ii
 = let
        -- The current module name.
        modName = interfaceModuleName ii

        -- Build a super declaration from an export.
        takeSuperOfExport ex@ExportValueLocal{}
         = Just $ Super
                { superName             = exportValueLocalName ex
                , superModuleName       = modName
                , superTetraType        = exportValueLocalType ex
                , superImportValue      = ImportValueModule
                                        { importValueModuleName  = modName
                                        , importValueModuleVar   = exportValueLocalName ex
                                        , importValueModuleType  = exportValueLocalType ex
                                        , importValueModuleArity = exportValueLocalArity ex }
                }

        takeSuperOfExport ex@ExportValueSea{}
         = Just $ Super
                { superName             = exportValueSeaNameInternal ex
                , superModuleName       = modName
                , superTetraType        = exportValueSeaType ex
                , superImportValue      = ImportValueSea
                                        { importValueSeaVar  = Text.unpack $ exportValueSeaNameExternal ex
                                        , importValueSeaType = exportValueSeaType ex }
                }

        takeSuperOfExport _
         = Nothing

        -- Build a super decalaration from an import.
{-      takeSuperOfImport im@ImportValueModule{}
         = Super
                { superName             = importValueModuleVar im
                , superModuleName       = modName
                , superTetraType        = importValueModuleType im
                , superImportValue      = im }

        takeSuperOfImport im@ImportValueSea{}
         = Super
                { superName             = D.NameVar $ importValueSeaVar im
                , superModuleName       = modName
                , superTetraType        = importValueSeaType im
                , superImportValue      = im }
-}

   in   Map.fromList
                [ (n, let Just s = takeSuperOfExport ex in s)
                | (n, ex) <- moduleExportValues mmDiscus]
{-
         (Map.fromList
                [ (n, takeSuperOfImport im)
                | (n, im) <- moduleImportValues mmDiscus])
-}

 | otherwise
 = Map.empty

