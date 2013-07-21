
-- | Unix-style command line interface to DDC.
--    This interface exports commands that work on whole modules at a time,
--    and does unix-style command line argument parsing.
--
import DDC.Main.Config
import DDC.Main.Help
import DDC.Main.Args
import DDC.Main.OptLevels
import DDC.Driver.Command.Parse
import DDC.Driver.Command.Check
import DDC.Driver.Command.Load
import DDC.Driver.Command.Compile
import DDC.Driver.Command.Make
import DDC.Driver.Command.Ast
import DDC.Driver.Command.BaseBuild

import DDC.Driver.Command.Flow.Prep
import DDC.Driver.Command.Flow.Lower
import DDC.Driver.Command.Flow.Concretize
import DDC.Driver.Command.Flow.Thread
import DDC.Driver.Command.Flow.Wind

import DDC.Driver.Command.ToSalt
import DDC.Driver.Command.ToC
import DDC.Driver.Command.ToLlvm
import DDC.Driver.Source
import DDC.Build.Builder
import DDC.Build.Language
import DDC.Base.Pretty
import System.Environment
import System.IO
import System.Exit
import System.FilePath
import Control.Monad.Trans.Error
import qualified DDC.Driver.Stage       as Driver
import qualified DDC.Core.Salt.Runtime  as Runtime


main :: IO ()
main
 = do   args    <- getArgs

        -- Get the default configuration.
        --  This contains static information such as where the code
        --  for the base libraries and runtime system is installed.
        config0 <- getDefaultConfig

        -- Update the static config with dynamic config read from
        --   the command-line arguments.
        config  <- parseArgs args config0

        -- Run the main compiler.
        run config


run :: Config -> IO ()
run config
 = case configMode config of
        -- We didn't get any arguments on the command line.
        ModeNone
         ->     putStr hello

        -- Display the version string.
        ModeVersion
         ->     putStrLn version

        -- Display the help page.
        ModeHelp
         ->     putStrLn help

        -- Parser a module.
        ModeParse filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdParseModule dconfig (SourceFile filePath) str

        -- Parse and type check a module.
        ModeCheck filePath
         -> do  language        <- languageFromFilePath filePath
                case language of 
                 Language bundle
                  -> do  mm      <- runErrorT 
                                 $ cmdCheckModuleFromFile (bundleFragment bundle) filePath
                         case mm of
                          Left err        
                           -> do putStrLn err
                                 exitWith $ ExitFailure 1

                          Right _
                           -> return ()

        -- Parse, type check and transform a module.
        ModeLoad filePath
         ->     runError $ cmdLoadFromFile 
                                (configTrans config) 
                                (configWith config) 
                                filePath

        -- Compile a module to object code.
        ModeCompile filePath
         -> do  dconfig  <- getDriverConfig config (Just filePath)
                runError $ cmdCompile dconfig filePath

        -- Compile a module into an executable.
        ModeMake filePath
         -> do  dconfig  <- getDriverConfig config (Just filePath)
                runError $ cmdMake    dconfig filePath

        -- Pretty print the AST of a module.
        ModeAST filePath
         -> do  language        <- languageFromFilePath filePath
                str             <- readFile filePath
                cmdAstModule 
                        language
                        (SourceFile filePath) 
                        str

        -- Convert a module to Salt.
        ModeToSalt filePath
         -> do  language        <- languageFromFilePath filePath
                dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdToSalt dconfig language (SourceFile filePath) str

        -- Convert a module to C
        ModeToC filePath
         -> do  language        <- languageFromFilePath filePath
                dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdToC dconfig language (SourceFile filePath) str

        -- Convert a module to LLVM
        ModeToLLVM filePath
         -> do  language        <- languageFromFilePath filePath
                dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdToLlvm dconfig language (SourceFile filePath) str

        -- Prepare a Disciple Core Flow program for lowering.
        ModeFlowPrep filePath
         -> do  dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdFlowPrep dconfig (SourceFile filePath) str

        -- Lower a Disciple Core Flow program to loops.
        ModeFlowLower filePath
         -> do  dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdFlowLower dconfig (SourceFile filePath) str

        -- Concretize rate type variables in a Disciple Core Flow program.
        ModeFlowConcretize filePath
         -> do  dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdFlowConcretize dconfig (SourceFile filePath) str

        -- Thread the world token through a Disciple Core Flow program.
        ModeFlowThread filePath
         -> do  dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdFlowThread dconfig (SourceFile filePath) str

        -- Wind loop primops into tail recursive loops.
        ModeFlowWind filePath
         -> do  dconfig         <- getDriverConfig config (Just filePath)
                str             <- readFile filePath
                runError $ cmdFlowWind dconfig (SourceFile filePath) str

        -- Build the runtime and base libraries.
        ModeBaseBuild
         -> do  dconfig         <- getDriverConfig config Nothing
                runError $ cmdBaseBuild dconfig

        -- Print the external builder info for this platform.
        ModePrintBuilder
         -> do  dconfig         <- getDriverConfig config Nothing
                putStrLn $ renderIndent $ ppr (Driver.configBuilder dconfig)

        -- Print where the runtime and base libraries are installed.
        ModePrintBaseDir
         ->     putStrLn $ configBaseDir config


-- | Get the compile driver from the config.
getDriverConfig :: Config -> Maybe FilePath -> IO Driver.Config
getDriverConfig config filePath
 = do   Just builder    <- determineDefaultBuilder (defaultBuilderConfig config)
        let runtimeConfig
             = Runtime.Config
             { Runtime.configHeapSize = configRuntimeHeapSize config }

        simplLite       <- getSimplLiteOfConfig config builder               filePath
        simplSalt       <- getSimplSaltOfConfig config builder runtimeConfig filePath

        return  $ Driver.Config
                { Driver.configDump                     = configDump config
                , Driver.configSimplLite                = simplLite
                , Driver.configSimplSalt                = simplSalt
                , Driver.configViaBackend               = configViaBackend config

                , Driver.configRuntime                  = runtimeConfig
                , Driver.configBuilder                  = builder
                , Driver.configSuppressCoreImports      = False
                , Driver.configSuppressHashImports      = False
                , Driver.configOutputFile               = configOutputFile config
                , Driver.configOutputDir                = configOutputDir  config 
                , Driver.configKeepLlvmFiles            = configKeepLlvmFiles config
                , Driver.configKeepSeaFiles             = configKeepSeaFiles  config
                , Driver.configKeepAsmFiles             = configKeepAsmFiles  config 
                , Driver.configTaintAvoidTypeChecks     = configTaintAvoidTypeChecks config }


-- | Determine the current language based on the file extension of this path, 
--   and slurp out a bundle of stuff specific to that language from the config.
languageFromFilePath :: FilePath -> IO Language
languageFromFilePath filePath
 = case languageOfExtension (takeExtension filePath) of
        Nothing 
         -> do  hPutStrLn stderr "Unknown file extension."
                exitWith $ ExitFailure 1

        Just ext 
         -> return ext


-- | Print errors to stderr and set the exit code.
runError :: ErrorT String IO () -> IO ()
runError m
 = do   result  <- runErrorT m
        case result of
         Left err       
          -> do hPutStrLn stderr err
                exitWith $ ExitFailure 1

         Right _
          -> return ()

