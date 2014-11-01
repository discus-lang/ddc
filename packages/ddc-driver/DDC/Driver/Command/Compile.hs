
module DDC.Driver.Command.Compile
        ( cmdCompile
        , cmdCompileRecursive )
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Data.Canned
import DDC.Build.Pipeline
import DDC.Build.Interface.Base
import DDC.Data.Token
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.IORef
import qualified DDC.Driver.Build.Locate        as Locate
import qualified DDC.Build.Builder              as Builder
import qualified DDC.Source.Tetra.Module        as SE
import qualified DDC.Source.Tetra.Lexer         as SE
import qualified DDC.Source.Tetra.Parser        as SE
import qualified DDC.Core.Pretty                as P
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Parser                as C
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Base.Parser                as BP
import qualified DDC.Version                    as Version

import DDC.Driver.Command.Flow.ToTetra
import qualified DDC.Core.Flow                  as Flow


---------------------------------------------------------------------------------------------------
-- | Recursively compile source modules into @.o@ files.
--
--   Like `cmdCompile`, but if the interface for a needed module is not already 
--   loaded then use the provided command to load or compile it. A recursive
--   make process can be constructed by looking up the file the corresponds
--   to the module, and calling cmdCompileRecursive again -- tying the knot.
--
--   Returns the interfaces that were provided, plus any that were constructed
--   or loaded when compiling this module.
--
cmdCompileRecursive
        :: Config                               -- ^ Build driver config.
        -> Bool                                 -- ^ Build an exectable.
        -> [InterfaceAA]                        -- ^ Currently loaded interfaces.
        -> FilePath                             -- ^ Path to file to compile
        -> ExceptT String IO [InterfaceAA]       -- ^ All loaded interfaces files.

cmdCompileRecursive config buildExe0 interfaces0 filePath0
 | takeExtension filePath0 == ".ds"
 = loop buildExe0 interfaces0 filePath0 []

 | otherwise
 = cmdCompile config buildExe0 interfaces0 filePath0

 where 
  loop  buildExe interfaces filePath 
        modNamesPath
   = do
        liftIO $ putStrLn $ "building " ++ filePath

        -- Check that the source file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src             <- liftIO $ readFile filePath

        -- Parse just the header of the module to determine what other modules
        -- it imports.
        modNamesNeeded  <- tasteNeeded filePath src

        liftIO $ putStrLn $ "needed " ++ (show $ modNamesNeeded)

        -- Recursively compile modules until we have all the interfaces required
        -- for the current one.
        let chase intsHave = do
                -- Names of all the modules that we have interfaces for.
                let modsNamesHave   
                        = map interfaceModuleName intsHave

                -- Names of modules that we are missing interfaces for.
                let missing     = filter (\m -> not $ elem m modsNamesHave) 
                                $ modNamesNeeded

                case missing of
                 -- If there are no missing interfaces then we're good to go.
                 []     -> return intsHave

                 -- Otherwise compile the first of the missing modules and try again.
                 m : _  -> do

                        -- Automatically look for modules in the base library.
                        let baseDirs 
                                =  configModuleBaseDirectories config
                                ++ [Builder.buildBaseSrcDir (configBuilder config)
                                        </> "tetra" </> "base"]

                        mfilePath   <- Locate.locateModuleFromPaths baseDirs m ".ds"

                        -- Check that we haven't tried to compile this module before
                        -- on a recursive path. This detects module import loops.
                        when (elem m modNamesPath)
                         $ throwE $ unlines
                         $  [ "! Cannot build recursive modules:" ]
                         ++ [ "    " ++ (P.renderIndent $ P.ppr mm) | mm <- modNamesPath ]

                        -- Compile the first of the dependencies.
                        interfaces' <- loop False intsHave mfilePath (modNamesPath ++ [m])

                        -- See if we've got them all.
                        chase interfaces'

        interfaces'     <- chase interfaces

        -- At this point we should have all the interfaces needed
        -- for the current module.
        cmdCompile config buildExe interfaces' filePath


---------------------------------------------------------------------------------------------------
-- | Compile a source module into a @.o@ file.
--
--   This produces an @.o@ file next to the source file, and may also produce
--   a @.di@ interface, depending on what sort of source file we're compiling.
-- 
--   Returns the same interfaces files provides, plus the new one for this module.
--
cmdCompile
        :: Config               -- ^ Build driver config.
        -> Bool                 -- ^ Build an executable.
        -> [InterfaceAA]        -- ^ Interfaces of modules we've already loaded.
        -> FilePath             -- ^ Path to file to compile
        -> ExceptT String IO [InterfaceAA]

cmdCompile config buildExe interfaces filePath
 = do   
        if buildExe 
         then liftIO $ putStrLn $ "* Compiling " ++ filePath ++ " as executable"
         else liftIO $ putStrLn $ "* Compiling " ++ filePath

        let ext         = takeExtension filePath
        let source      = SourceFile filePath

        -- Read in the source file.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        src             <- liftIO $ readFile filePath

        -- If we're building an executable, then get paths to the other object
        -- files that we need to link with.
        let otherObjs
                | buildExe
                = Just $ map (\i -> replaceExtension (interfaceFilePath i) "o") interfaces

                | otherwise
                = Nothing

        -- During complation of this module the intermediate code will be
        -- stashed in these refs. We will use the intermediate code to build
        -- the interface for this module.
        refTetra        <- liftIO $ newIORef Nothing
        refSalt         <- liftIO $ newIORef Nothing

        -- Use the file extension to decide what compilation pipeline to use.
        let make
                -- Compile a Source Tetra module.
                | ext == ".ds"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageSourceTetraLoad config source interfaces
                [ PipeCoreHacks      (Canned $ \m -> writeIORef refTetra (Just m) >> return m)
                [ PipeCoreReannotate (const ())
                [ stageTetraToSalt    config source pipesSalt ]]]

                -- Compile a Core Tetra module.
                | ext == ".dct"
                = liftIO
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageTetraLoad    config source
                [ stageTetraToSalt  config source pipesSalt ]

                -- Compile a Core Lite module.
                | ext == ".dcl"
                = liftIO 
                $ pipeText (nameOfSource source) (lineStartOfSource source) src
                $ stageLiteLoad     config source
                [ stageLiteOpt      config source  
                [ stageLiteToSalt   config source pipesSalt ]]

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
                let pathInterface   = replaceExtension filePath "di"
                let int = Interface
                        { interfaceVersion      = Version.version
                        , interfaceFilePath     = pathInterface
                        , interfaceModuleName   = mn
                        , interfaceTetraModule  = modTetra 
                        , interfaceSaltModule   = modSalt }

                liftIO  $ writeFile pathInterface 
                        $ P.renderIndent $ P.ppr int

                return (interfaces ++ [int])

          -- Compilation was successful,
          --  but we didn't get enough build products to produce an interface file.
          | otherwise
          -> return []


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

                let context 
                        = C.Context
                        { C.contextTrackedEffects         = True
                        , C.contextTrackedClosures        = True
                        , C.contextFunctionalEffects      = False
                        , C.contextFunctionalClosures     = False }

                case BP.runTokenParser C.describeTok filePath
                        (SE.pModule context) tokens of
                 Left  err  -> throwE $ P.renderIndent $ P.ppr err
                 Right mm   -> return $ SE.moduleImportModules mm

        | otherwise
        = return []


-- | Drop tokens after and including the first 'where' keyword.
--   When parsing just the module header we can drop these tokens
--   because they only represent the body of the module.
dropBody :: [Token (C.Tok n)] -> [Token (C.Tok n)]
dropBody toks = go toks
 where  go []                                           = []
        go (Token { tokenTok = C.KA C.KWhere} : _)      = []
        go (t : moar)                                   = t : go moar

