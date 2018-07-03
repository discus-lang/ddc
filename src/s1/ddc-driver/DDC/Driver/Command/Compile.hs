module DDC.Driver.Command.Compile
        ( cmdCompileRecursive
        , cmdCompileRecursiveDS
--        , cmdLoadOrCompile
        , cmdCompile
        , getModificationTimeIfExists)
where
import DDC.Driver.Stage
import qualified DDC.Driver.Stage.Tetra         as DE
import qualified DDC.Driver.Stage.Salt          as DA

import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import DDC.Core.Interface.Base
import qualified DDC.Data.Pretty                as P

import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Time.Clock
import qualified DDC.Build.Interface.Locate             as Locate
import qualified DDC.Build.Builder                      as Builder
import qualified DDC.Source.Discus.Module               as SE
import qualified DDC.Source.Discus.Lexer                as SE
import qualified DDC.Source.Discus.Parser               as SE
-- import qualified DDC.Core.Codec.Text.Pretty             as P
import qualified DDC.Core.Codec.Text.Lexer              as C

import qualified DDC.Core.Codec.Shimmer.Encode          as C.Encode
import qualified DDC.Core.Discus.Codec.Shimmer.Encode   as D.Encode
-- import qualified DDC.Core.Discus.Codec.Shimmer.Decode   as D.Decode

import qualified DDC.Core.Salt.Runtime                  as A
import qualified DDC.Core.Discus                        as D
import qualified DDC.Core.Module                        as C
import qualified DDC.Control.Parser                     as BP
import qualified DDC.Version                            as Version
import qualified Data.List                              as List
import qualified Data.Text                              as T

import qualified DDC.Core.Transform.Reannotate                  as CReannotate

import DDC.Core.Interface.Store                                 (Store)
-- import qualified DDC.Core.Codec.Shimmer.Encode          as IntShimmer
import qualified DDC.Core.Interface.Store                       as Store


---------------------------------------------------------------------------------------------------
-- | Jobs we need to do in the compilation.
data Job
        -- | Find and load a Discus module.
        = JobLoadDiscusModule   C.ModuleName

        -- | Build the source file at the given path.
        | JobBuildFilePath      FilePath
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
-- | Recursively compile source modules into @.o@ files,
--   or load existing interfaces if we have them and the @.o@ file is
--   still fresh.
--
--   * Interface files that are loaded or generated during compilation
--     are added to the interface store.
--
cmdCompileRecursive
        :: Config               -- ^ Build driver config.
        -> Bool                 -- ^ Build an exectable.
        -> Store D.Name         -- ^ Interface store.
        -> [FilePath]           -- ^ Paths of files to compile.
        -> ExceptT String IO ()

cmdCompileRecursive config bBuildExe store fsPath
 -- Recursively build a source program and link with some extra objects.
 | all (\f -> elem (takeExtension f) [".ds", ".o"]) fsPath
 = do
        -- Check that all the files exists before we try to compile any.
        -- We particuarly want to check the .o files are there before
        -- we start compiling any .ds files.
        forM_ fsPath $ \fPath
         -> do  exists <- liftIO $ doesFileExist fPath
                when (not exists)
                 $ throwE $ "No such file " ++ show fPath

        -- Split file list into souce files and extra objects.
        let fsDS  = filter (\f -> takeExtension f == ".ds") fsPath
        let fsO   = filter (\f -> takeExtension f == ".o")  fsPath

        -- Start recursive build.
        cmdCompileRecursiveDS
                config bBuildExe fsO store
                (map JobBuildFilePath fsDS)
                []

 | otherwise
 = do   mapM_ (cmdCompile config bBuildExe [] store) fsPath


---------------------------------------------------------------------------------------------------
-- | Recursively compile @.ds@ source modules into @.o@ files,
--   or load existing interfaces if we have them and the @.o@ file is
--   still fresh.
--
--   * Interface files that are loaded or generated during compilation
--     are added to the interface store.
--
cmdCompileRecursiveDS
        :: Config       -- ^ Build driver config.
        -> Bool         -- ^ Build an executable.
        -> [FilePath]   -- ^ Extra object files to link with.
        -> Store D.Name -- ^ Inferface store.
        -> [Job]        -- ^ Jobs we still need to perform.
        -> [Job]        -- ^ Jobs that we have already attempted in the context.
        -> ExceptT String IO ()

cmdCompileRecursiveDS _config _bBuildExe _fsO _store [] _jsBlocked
 = return ()

cmdCompileRecursiveDS config bBuildExe fsO store (jNext : jsMore) jsBlocked

 -- Load a named Discus module, either from an existing interface,
 -- or try to locate and build the source file.
 | JobLoadDiscusModule nModule <- jNext
 = do   -- Try to load an existing interface file.
        -- This will return True if we find an interface file,
        --  *and* it is fresh relative to the associated source file.
        bFoundFresh <- liftIO $ Store.loadInterface store nModule
        if bFoundFresh
         then   cmdCompileRecursiveDS config bBuildExe fsO store
                        jsMore jsBlocked
         else do
                -- We can't load the interface, so try to build the module
                -- from source instead.
                path <- locateModuleFromConfig config nModule
                cmdCompileRecursiveDS config bBuildExe fsO store
                        (JobBuildFilePath path : jsMore) jsBlocked

 -- Build
 | JobBuildFilePath filePath <- jNext
 = do
        -- Check if the requested file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src             <- liftIO $ readFile filePath

        -- Parse just the header of the module to determine what
        -- other modules it imports.
        modNamesNeeded  <- tasteNeeded filePath src

        -- Names of all the modules that we currently have interfaces for.
        modsNamesHave   <- liftIO $ Store.getModuleNames store

        -- Names of modules that we are missing interfaces for.
        let missing     = filter (\m -> not $ elem m modsNamesHave)
                        $ modNamesNeeded

--         liftIO  $ putStr $ unlines
--                 [ "Modules Needed  = " ++ show modNamesNeeded
--                 , "Modules Have    = " ++ show modsNamesHave
--                 , "Modules Missing = " ++ show missing ]

        case missing of
         -- We've already got all the interfaces needed by the
         -- current module.
         [] -> do
                -- Compile the current module.
                cmdCompile config bBuildExe fsO store filePath

                -- Perform the other jobs that are still queued.
                cmdCompileRecursiveDS config bBuildExe fsO store jsMore []

         -- We still need to load or compile dependent modules.
         ms -> do
                -- Determine filepaths for all dependent modules.
                let jsDep = map JobLoadDiscusModule ms

                -- Check that we're not on a recursive loop,
                -- trying to compile a module that's importing itself.
{-              FIXME: fix the recursive check, note multiple reps
                       of job moduleName vs filePath

                let fsRec = List.intersect fsMore fsBlocked
                when (not $ null fsRec)
                 $ throwE $ unlines
                 $  [ "Cannot build recursive module" ]
                 ++ [ "    " ++ show fsRec ]
-}
                -- Shift the current module to the end of the queue,
                -- compiling the dependent modules first.
                cmdCompileRecursiveDS config bBuildExe fsO store
                        (List.nub $ jsDep ++ jsMore ++ [jNext])
                        (JobBuildFilePath filePath : jsBlocked)


---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- | Compile a source module into a @.o@ file.
--
--   * Interfaces for dependent modules must already be in the interface
--     store.
--
--   * This produces an @.o@ file next to the source file, and may also
--     produce a @.di@ interface, depending on what sort of source file
--     we're compiling.
--
--   * If compilation produces an interface then it is added to the
--     existing store.
--
cmdCompile
        :: Config               -- ^ Build driver config.
        -> Bool                 -- ^ Build an executable.
        -> [FilePath]           -- ^ Extra object files to link with.
        -> Store D.Name         -- ^ Interface store.
        -> FilePath             -- ^ Path to source file.
        -> ExceptT String IO ()

cmdCompile config bBuildExe' fsExtraO store filePath
 = withExceptT (P.renderIndent . P.vcat . map P.ppr)
 $ do
        let bBuildExe
                =  takeBaseName filePath == "Main" && bBuildExe'

        if bBuildExe
         then liftIO $ putStrLn $ "* Compiling " ++ filePath ++ " as executable"
         else liftIO $ putStrLn $ "* Compiling " ++ filePath

        let ext         = takeExtension filePath
        let source      = SourceFile filePath

        -- Read in the source file.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE [ErrorLoad $ "No such file " ++ show filePath]

        src     <- liftIO $ readFile filePath

        -- If we're building an executable, then get paths to the other object
        -- files that we need to link with.
        metas           <- liftIO $ Store.getMeta store
        let pathsDI     =  map Store.metaFilePath metas
        let otherObjs
                | bBuildExe = Just $  map (\path -> replaceExtension path "o") pathsDI
                                   ++ fsExtraO
                | otherwise = Nothing

        -- Determine directory for build products.
        let (pathO, _)  = objectPathsOfConfig config filePath
        let pathDI      = replaceExtension pathO ".di"
        liftIO $ createDirectoryIfMissing True (takeDirectory pathO)

        -- Load text source and compile to a Core Tetra file, if appropriate.
        let makeTetra
                -- Load a Source Tetra module to Core Tetra.
                | ext == ".ds"
                = fmap Just $ DE.sourceLoadText config store source src

                -- Load a Core Tetra module.
                | ext == ".dct"
                = fmap Just $ DE.discusLoadText config store source src

                -- Load a Core Salt module
                | ext == ".dcs"
                = return Nothing

                | otherwise
                = throwE [ErrorLoad $ "Cannot compile '" ++ ext ++ "' files."]

        mModTetra <- makeTetra

        -- If we're compiling a Main.ds module then automatically inject
        -- the default exception handler.
        let config_handler
                | ext == ".ds"
                = config
                { configRuntime
                    = (configRuntime config)
                    { A.configHookHandleTopLevel
                        = Just (T.pack "Console", T.pack "ddcHookHandleTopLevel") } }

                | otherwise
                = config

        -- Convert Core Tetra to Core Salt.
        --   For Discus source files we pass in the names of modules reachable
        --   from the current one. This is done so we can call the initialization
        --   function for each module from the Main module.
        mnsTrans <- liftIO $ Store.getModuleNames store
        let makeSalt
                | ext == ".dcs"
                = fmap (CReannotate.reannotate (const ()))
                $ DA.saltSimplify config source
                =<< DA.saltLoadText config source src

                | Just modTetra <- mModTetra
                = DE.discusToSalt  config_handler source mnsTrans
                $ CReannotate.reannotate (const ()) modTetra

                | otherwise
                = throwE [ErrorLoad ("no tetra file" :: String)]

        modSalt <- makeSalt

        -- Convert Core Salt into object code.
        let bSlotify
                = case ext of
                        ".dcs"  -> False
                        _       -> True

        (case configViaBackend config of
             ViaLLVM -> DA.saltCompileViaLlvm config source otherObjs
                                bSlotify bBuildExe modSalt)


        -- Get current time stamp for interface file.
        timeDI  <- liftIO $ getCurrentTime

        case mModTetra of
         Nothing        -> return ()
         Just modTetra
          -> do
                -- Write out the interface file.
                let int = Interface
                        { interfaceVersion      = Version.version
                        , interfaceFilePath     = pathDI
                        , interfaceTimeStamp    = timeDI
                        , interfaceModuleName   = C.moduleName modSalt
                        , interfaceModule       = CReannotate.reannotate (const ()) modTetra }

                let cEncode
                        = C.Encode.Config
                        { C.Encode.configTakeRef        = D.Encode.takeName
                        , C.Encode.configTakeVarName    = D.Encode.takeVarName
                        , C.Encode.configTakeConName    = D.Encode.takeConName }

                liftIO  $ C.Encode.storeInterface cEncode pathDI int

                -- Add the new interface to the store.
                liftIO $ Store.addInterface store int

                return ()


---------------------------------------------------------------------------------------------------
-- Taste the header of the module to see what other modules it depends on.
--  Only Source modules can import other modules.
--  For core modules, all the required information is listed explicitly
--  in the module itself.
tasteNeeded
        :: FilePath             -- ^ Path of module.
        -> String               -- ^ Module source.
        -> ExceptT String IO [C.ModuleName]

tasteNeeded filePath src
 | takeExtension filePath == ".ds"
 = do
        -- Lex the module, dropping all tokens after and including
        -- the first 'where', because we only need the module header.
        let tokens
                = dropBody
                $ SE.lexModuleString filePath 1 src

        case BP.runTokenParser
                C.describeToken
                filePath SE.pModule tokens of
         Left  err  -> throwE $ P.renderIndent $ P.ppr err
         Right mm
          -> do
                -- Check that the module name matches the file path where
                -- we found the module. If they don't match then the compilation
                -- driver will go into a loop as it can never load a module
                -- with the name it needs.
                when (not $ C.moduleNameMatchesPath filePath (SE.moduleName mm))
                 $ error $ unlines
                 [ "Module name does not match file path."
                 , "  module name = " ++ show (SE.moduleName mm)
                 , "  file path   = " ++ show filePath ]

                return $ SE.moduleImportModules mm

 | otherwise
 = return []


---------------------------------------------------------------------------------------------------
-- | Given a driver config, locate the module with the given name.
locateModuleFromConfig
        :: Config
        -> C.ModuleName
        -> ExceptT String IO FilePath

locateModuleFromConfig config mname
 = do   -- Automatically look for modules in the base library.
        let baseDirs
                =  configModuleBaseDirectories config
                ++ [Builder.buildBaseSrcDir (configBuilder config) </> "base"]

        liftIO (Locate.locateModuleFromPaths baseDirs mname "source" ".ds")
         >>= \case
                Left  err  -> throwE $ P.renderIndent $ P.ppr err
                Right path -> return path


-- | If the given file exists then get its modification time,
--   otherwise Nothing.
getModificationTimeIfExists :: FilePath -> IO (Maybe UTCTime)
getModificationTimeIfExists path
 = do   exists  <- doesFileExist path
        if exists
         then do
                timeStamp <- getModificationTime path
                return $ Just timeStamp

         else   return Nothing


-- | Drop tokens after and including the first 'where' keyword.
--   When parsing just the module header we can drop these tokens
--   because they only represent the body of the module.
dropBody :: [C.Located (C.Token n)] -> [C.Located (C.Token n)]
dropBody toks = go toks
 where  go []           = []

        go (C.Located _ (C.KA (C.KKeyword C.EWhere)) : _)
                        = []

        go (t : moar)   = t : go moar


-------------------------------------------------------------------------------
-- | Look for an interface file in the given directory.
--   If there's one there see if it's fresh enough to reload (True)
--   or whether we need to rebuild it (False).
--
--  It's safe to reload the module from an inteface file if:
--   1. There is an existing interface which is fresher than the source.
--   2. There is an existing object    which is fresher than the source.
--   3. There is an existing interface which is fresher than the
--      interfaces of all dependencies.
--
--  Additionally, we force rebuild for the top level module, because
--  that's what was mentioned on the command line. We're trying to
--  follow the principle of least surprise in this regard.
--
{- FIXME: we need to reinstate this in the module source locator.
interfaceIsFresh
        :: Store D.Name         -- ^ Current interface store.
        -> UTCTime              -- ^ Timestamp on original source file.
        -> [C.ModuleName]       -- ^ Names of modules needed by source.
        -> FilePath             -- ^ Expected path of object file.
        -> IO Bool

interfaceIsFresh store timeDS modNamesNeeded filePathO
 = do
        let filePathDI  =  replaceExtension filePathO ".di"
        mTimeO          <- liftIO $ getModificationTimeIfExists filePathO
        mTimeDI         <- liftIO $ getModificationTimeIfExists filePathDI
        meta'           <- liftIO $ Store.getMeta store

        -- object is fresher than source
        let bFreshO
                | Just timeO  <- mTimeO,  timeDS < timeO  = True
                | otherwise                               = False

        -- interface is fresher than source.
        let bFreshDI
                | Just timeDI <- mTimeDI, timeDS < timeDI = True
                | otherwise                               = False

        let bFreshDep
                | Just timeDI <- mTimeDI
                = and   [ Store.metaTimeStamp m <= timeDI
                                | m <- meta'
                                , elem (Store.metaModuleName m) modNamesNeeded ]

                | otherwise
                = False

        return  $ and [bFreshO, bFreshDI, bFreshDep]
-}

-- | Load the interface correponding to a source file,
--   or re-compile the source if it's fresher than the interface.
--
--   * Interfaces for dependent modules must already be in the
--     interface store.
--
{- FIXME: make sure we're using the aux source libs.
cmdLoadOrCompile
        :: Config               -- ^ Build driver config.
        -> Bool                 -- ^ Build an exeecutable.
        -> [FilePath]           -- ^ Extra object files to link with.
        -> Store D.Name         -- ^ Interface store.
        -> FilePath             -- ^ Path to source file.
        -> ExceptT String IO ()

cmdLoadOrCompile config buildExe fsO store filePath
 = do
        -- Check that the source file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file and get the current timestamp.
        src             <- liftIO $ readFile filePath
        Just timeDS     <- liftIO $ getModificationTimeIfExists filePath

        -- Parse just the header of the module to determine
        -- what other modules it imports.
        modNamesNeeded  <- tasteNeeded filePath src

        -- Search through the likely paths that might hold a pre-compiled
        -- interface and object file. If we find it then we can reload it,
        -- otherwise we'll need to build the module from source again.
        let search (filePathO : filePathsMoreO)
             = do
                   -- The .di file for the same module should be next to
                   -- any .o file for it.
--                   let filePathDI = replaceExtension filePathO ".di"

                   -- Check if we have a fresh interface and object
                   -- at this path.
                   fresh <- liftIO $ interfaceIsFresh store timeDS modNamesNeeded filePathO

                   -- If we indeed have a fresh interface and object then we can
                   -- load it directly. Otherwise search the rest of the paths.
                   if fresh && not (takeFileName filePath == "Main.ds")
                    then do
--                         liftIO  $ putStrLn $ "* Loading "  ++ filePathDI
                        result  <- Store.loadInterface store D.Decode.takeName filePathDI
                        let result = error "cmdLoadOrCompile: load interface"
                        case result of
                          Left  _err -> throwE $ P.renderIndent $ P.string "FIXME" -- $ P.ppr err
                          Right int  -> liftIO $ Store.addInterface store int

                    else search filePathsMoreO

            -- We're out of places to search for pre-existing interface
            -- files, so build it gain.
            search []
             = do
--                  liftIO  $ putStrLn "* Compiling"
                  cmdCompile config buildExe fsO store filePath

        -- Check the config for where the interface might be.
        -- It'll either be next to the source file or in the auxilliary
        -- output directory if that was specified.
        let (filePathO_output, filePathO_libs)
                =  objectPathsOfConfig config filePath

        -- Search all the likely places.
        search (filePathO_output : filePathO_libs)
-}


-- [Note: Timestamp acccuracy during rebuild]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- There's an ugly system issue where if the underlying file system does
-- not support file time stamps with sub-second accuracy, then the
-- timestamps of the interface files we compile in this run will have more
-- accuracy than the ones we load from the file system.
--
-- The problem with inaccurate timestamps is that if we compiled two
-- dependent modules within the same second, then both will have the
-- same time-stamp and none is fresher than the other.
--
-- Due to this we allow the time stamp of dependent interface files to
-- be equal so that they will not be rebuilt in this situation.
--
-- We assume that if any process legitimately changes a dependent
-- object file then this will be done at least a second after we first
-- created it.
--
