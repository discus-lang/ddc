
module DDC.Driver.Command.Compile
        ( cmdCompileRecursive
        , cmdCompileRecursiveDS
        , cmdLoadOrCompile
        , cmdCompile
        , getModificationTimeIfExists)
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import DDC.Build.Interface.Base
import DDC.Data.Canned
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.IORef
import qualified DDC.Driver.Build.Locate        as Locate
import qualified DDC.Build.Builder              as Builder
import qualified DDC.Source.Tetra.Module        as SE
import qualified DDC.Source.Tetra.Lexer         as SE
import qualified DDC.Source.Tetra.Parser        as SE
import qualified DDC.Core.Pretty                as P
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Base.Parser                as BP
import qualified DDC.Version                    as Version
import qualified Data.List                      as List

import DDC.Driver.Command.Flow.ToTetra
import qualified DDC.Core.Flow                  as Flow

import DDC.Build.Interface.Store                (Store)
import qualified DDC.Build.Interface.Store      as Store


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
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Path to file to compile
        -> ExceptT String IO ()

cmdCompileRecursive config bBuildExe store filePath
 | takeExtension filePath == ".ds"
 = do   cmdCompileRecursiveDS config bBuildExe store [filePath] []

 | otherwise
 = do   cmdCompile            config bBuildExe store filePath


---------------------------------------------------------------------------------------------------
-- | Recursively compile @.ds@ source modules into @.o@ files,
--   or load existing interfaces if we have them and the @.o@ file is
--   still fresh.
--
--   * Interface files that are loaded or generated during compilation
--     are added to the interface store.
--
cmdCompileRecursiveDS
        :: Config               -- ^ Build driver config.
        -> Bool                 -- ^ Build an executable.
        -> Store                -- ^ Inferface store.
        -> [FilePath]           -- ^ Names of source files still to load.
        -> [FilePath]           -- ^ Names of source files currently blocked.
        -> ExceptT String IO ()

cmdCompileRecursiveDS _config _bBuildExe _store []           _fsBlocked
 = return ()

cmdCompileRecursiveDS  config  bBuildExe  store (filePath:fs) fsBlocked
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

        -- Names of all the modules that we have interfaces for.
        modsNamesHave   <- liftIO $ Store.getModuleNames store

        -- Names of modules that we are missing interfaces for.
        let missing     = filter (\m -> not $ elem m modsNamesHave) 
                        $ modNamesNeeded

        case missing of
         -- We've already got all the interfaces needed by the
         -- current module.
         [] -> do
                -- Compile the current module.
                cmdLoadOrCompile config bBuildExe store filePath

                -- Build other modules that are still queued.
                cmdCompileRecursiveDS config bBuildExe store fs []

         -- We still need to load or compile dependent modules.
         ms -> do
                -- Determine filepaths for all dependent modules.
                fsMore  <- mapM (locateModuleFromConfig config) ms

                -- Check that we're not on a recursive loop, 
                -- trying to compile a module that's importing itself.
                let fsRec = List.intersect fsMore fsBlocked
                when (not $ null fsRec)
                 $ throwE $ unlines
                 $  [ "Cannot build recursive module" ]
                 ++ [ "    " ++ show fsRec ]

                -- Shift the current module to the end of the queue, 
                -- compiling the dependent modules first.
                cmdCompileRecursiveDS config bBuildExe store 
                        (List.nub $ fsMore ++ fs ++ [filePath]) 
                        (filePath : fsBlocked)


---------------------------------------------------------------------------------------------------
-- | Load the interface correponding to a source file,
--   or re-compile the source if it's fresher than the interface.
--
--   * Interfaces for dependent modules must already be in the
--     interface store.
--
cmdLoadOrCompile
        :: Config               -- ^ Build driver config.
        -> Bool                 -- ^ Build an exeecutable.
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Path to source file.
        -> ExceptT String IO ()

cmdLoadOrCompile config buildExe store filePath
 = do
        -- Check that the source file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src             <- liftIO $ readFile filePath
        Just timeDS     <- liftIO $ getModificationTimeIfExists filePath

        -- Parse just the header of the module to determine what other modules
        -- it imports.
        modNamesNeeded  <- tasteNeeded filePath src

        -- It's safe to reload the module from an inteface file if:
        --  1. There is an existing interface which is fresher than the source.
        --  2. There is an existing object    which is fresher than the source.
        --  3. There is an existing interface which is fresher than the 
        --     interfaces of all dependencies.
        --
        -- Additionally, we force rebuild for the top level module, because
        -- that's what was mentioned on the command line. We're trying to
        -- follow the principle of least surprise in this regard.
        --
        let filePathO   =  objectPathOfConfig config filePath
        let filePathDI  =  replaceExtension filePathO ".di"
        mTimeO          <- liftIO $ getModificationTimeIfExists filePathO
        mTimeDI         <- liftIO $ getModificationTimeIfExists filePathDI
        meta'           <- liftIO $ Store.getMeta store

        let loadOrCompile
                -- object and interface are fresher than source.
                | Just timeO    <- mTimeO,      timeDS < timeO
                , Just timeDI   <- mTimeDI,     timeDS < timeDI

                  -- Interface at least as fresh than all dependencies.
                  -- See Note: Timestamp accuracy during rebuild.
                , and   [ Store.metaTimeStamp m <= timeDI 
                                | m <- meta'
                                , elem (Store.metaModuleName m) modNamesNeeded ]

                  -- this is not the top-level module.
                , not $ takeFileName filePath == "Main.ds"
                = do    result  <- liftIO $ Store.load filePathDI
                        case result of
                         Left  err -> throwE $ P.renderIndent $ P.ppr err
                         Right int -> liftIO $ Store.wrap store int

                | otherwise
                = do    cmdCompile config buildExe store filePath

        loadOrCompile


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
        -> Store                -- ^ Interface store.
        -> FilePath             -- ^ Path to source file.
        -> ExceptT String IO ()

cmdCompile config bBuildExe' store filePath
 = do   
        let buildExe
                =  takeBaseName filePath == "Main"
                && bBuildExe'

        if buildExe 
         then liftIO $ putStrLn $ "* Compiling " ++ filePath ++ " as executable"
         else liftIO $ putStrLn $ "* Compiling " ++ filePath

        let ext         = takeExtension filePath
        let source      = SourceFile filePath

        -- Read in the source file.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        src     <- liftIO $ readFile filePath

        -- If we're building an executable, then get paths to the other object
        -- files that we need to link with.
        metas   <- liftIO $ Store.getMeta store
        let pathsDI     = map Store.metaFilePath metas
        let otherObjs
                | buildExe  = Just $ map (\path -> replaceExtension path "o") pathsDI
                | otherwise = Nothing

        -- During complation of this module the intermediate code will be
        -- stashed in these refs. We will use the intermediate code to build
        -- the interface for this module.
        refTetra <- liftIO $ newIORef Nothing
        refSalt  <- liftIO $ newIORef Nothing

        -- Use the file extension to decide what compilation pipeline to use.
        let make
                -- Compile a Source Tetra module.
                | ext == ".ds"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSourceTetraLoad config source store
                [ PipeCoreHacks      (Canned $ \m -> writeIORef refTetra (Just m) >> return m)
                [ PipeCoreReannotate (const ())
                [ stageTetraToSalt    config source pipesSalt ]]]

                -- Compile a Core Tetra module.
                | ext == ".dct"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageTetraLoad    config source
                [ stageTetraToSalt  config source pipesSalt ]

                -- Compile a Core Salt module.
                | ext == ".dcs"
                = liftIO 
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSaltLoad     config source pipesSalt

                -- Compile a Core Salt module.
                | ext == ".dcf"
                = liftIO 
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ pipelineFlowToTetra config Flow.defaultConfigScalar source pipesSalt

                -- Unrecognised.
                | otherwise
                = throwE $ "Cannot compile '" ++ ext ++ "' files."

            pipesSalt
             = case configViaBackend config of
                ViaLLVM
                 -> [ PipeCoreReannotate (const ())
                    [ stageSaltOpt     config source
                    [ PipeCoreHacks    (Canned $ \m -> writeIORef refSalt (Just m) >> return m)
                    [ stageSaltToLLVM  config source 
                    [ stageCompileLLVM config source filePath otherObjs ]]]]]

                ViaC
                 -> [ PipeCoreReannotate (const ())
                    [ stageSaltOpt     config source
                    [ stageCompileSalt config source filePath False ]]]

        -- Run the compilation pipeline.
        errs <- make


        -- Read back intermediate code from our refs.
        --   This will be written out as part of the interface file for this module.
        modTetra  <- liftIO $ readIORef refTetra
        modSalt   <- liftIO $ readIORef refSalt

        -- Handle errors ------------------------
        case errs of
         -- There was some error during compilation.
         es@(_:_)     
          -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es

         -- Compilation was successful, 
         --  but we need to have produced at least a Tetra or Salt module
         --  before we can build the interface file.
         []    
          | Just (mn : _)       
                <- sequence 
                        [ liftM C.moduleName modTetra
                        , liftM C.moduleName modSalt ]
          -> do
                -- write out the interface file.
                let pathO       = objectPathOfConfig config filePath
                let pathDI      = replaceExtension pathO ".di"

                timeDI  <- liftIO $ getCurrentTime 
                let int = Interface
                        { interfaceVersion      = Version.version
                        , interfaceFilePath     = pathDI
                        , interfaceTimeStamp    = timeDI
                        , interfaceModuleName   = mn
                        , interfaceTetraModule  = modTetra 
                        , interfaceSaltModule   = modSalt  }

                liftIO  $ writeFile pathDI
                        $ P.renderIndent $ P.ppr int

                -- Add the new interface to the store.
                liftIO $ Store.wrap store int

                return ()

          -- Compilation was successful,
          --  but we didn't get enough build products to produce an interface file.
          | otherwise
          -> return ()


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

        case BP.runTokenParser C.describeTok filePath SE.pModule tokens of
         Left  err  -> throwE $ P.renderIndent $ P.ppr err
         Right mm   -> return $ SE.moduleImportModules mm

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
                ++ [Builder.buildBaseSrcDir (configBuilder config)
                        </> "tetra" </> "base"]

        Locate.locateModuleFromPaths baseDirs mname ".ds"


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
dropBody :: [C.Located (C.Tok n)] -> [C.Located (C.Tok n)]
dropBody toks = go toks
 where  go []           = []

        go (C.Located _ (C.KA (C.KKeyword C.EWhere)) : _)
                        = []

        go (t : moar)   = t : go moar


-- [Note: Timestamp acccuracy during rebuild]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- There's an ugly system where if the underlying file system does not
-- support file time stamps with sub-second accuracy, then the timestamps
-- of the interface files we compile in this run will have more accuracy
-- than the ones we load from the file system.
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
