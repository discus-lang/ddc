
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
import DDC.Driver.Command.Build
import DDC.Driver.Command.BaseBuild

import DDC.Driver.Command.Tetra.Suspend
import DDC.Driver.Command.Tetra.Curry
import DDC.Driver.Command.Tetra.Boxing

import DDC.Driver.Command.Flow.Prep
import DDC.Driver.Command.Flow.Lower
import DDC.Driver.Command.Flow.Concretize
import DDC.Driver.Command.Flow.Melt
import DDC.Driver.Command.Flow.Wind
import DDC.Driver.Command.Flow.Thread
import qualified DDC.Core.Flow          as Flow

import DDC.Driver.Command.ToSalt
import DDC.Driver.Command.ToC
import DDC.Driver.Command.ToLlvm
import DDC.Driver.Command.ToPHP

import DDC.Interface.Source
import DDC.Build.Builder
import DDC.Base.Pretty
import System.Environment
import System.IO
import System.Exit
import Control.Monad.Trans.Except
import qualified DDC.Driver.Stage               as Driver
import qualified DDC.Driver.Config              as Driver
import qualified DDC.Core.Salt.Runtime          as Runtime
import qualified DDC.Core.Simplifier.Recipe     as Simplifier
import qualified DDC.Version                    as Version
import qualified DDC.Build.Interface.Store      as Store


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
         ->     putStrLn Version.splash

        -- Display the help page.
        ModeHelp
         ->     putStrLn help

        -- Parse a module.
        ModeParse filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                runError $ cmdParseFromFile dconfig filePath

        -- Parse and type check a module.
        ModeCheck filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                result  <- runExceptT $  cmdCheckFromFile dconfig store filePath
                
                case result of
                 Left err        
                  -> do putStrLn err
                        exitWith $ ExitFailure 1

                 Right ()
                  -> return ()

        -- Parse, type check and transform a module.
        ModeLoad filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdLoadFromFile dconfig store
                                (configTrans config) (configWith config) 
                                filePath

        -- Compile a module to object code.
        ModeCompile filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdCompileRecursive dconfig False store filePath []

        -- Compile a module into an executable.
        ModeMake filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdCompileRecursive dconfig True store filePath []

        -- Build libraries or executables following a .spec file.
        ModeBuild filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdBuild dconfig store filePath

        -- Convert a module to Salt.
        ModeToSalt filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdToSaltFromFile dconfig store filePath

        -- Convert a module to C
        ModeToC filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdToSeaFromFile  dconfig store filePath

        -- Convert a module to LLVM
        ModeToLLVM filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdToLlvmFromFile dconfig store filePath

        -- Convert a module to C
        ModeToPHP filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                store   <- Store.new
                runError $ cmdToPHPFromFile  dconfig store filePath


        -- Tetra specific -----------------------------------------------------
        ModeTetraSuspend filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdTetraSuspend dconfig (SourceFile filePath) str

        ModeTetraCurry filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdTetraCurry dconfig (SourceFile filePath) str

        ModeTetraBoxing filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdTetraBoxing dconfig (SourceFile filePath) str

        -- Flow specific ------------------------------------------------------
        -- Prepare a Disciple Core Flow program for lowering.
        ModeFlowPrep filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdFlowPrep dconfig (SourceFile filePath) str

        -- Lower a Disciple Core Flow program to loops.
        ModeFlowLower filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError 
                 $ cmdFlowLower dconfig Flow.defaultConfigScalar
                        (SourceFile filePath) str

        -- Lower a Disciple Core Flow program to loops.
        ModeFlowLowerKernel filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError 
                 $ cmdFlowLower dconfig Flow.defaultConfigKernel
                        (SourceFile filePath) str

        -- Lower a Disciple Core Flow program to loops.
        ModeFlowLowerVector filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError 
                 $ cmdFlowLower dconfig Flow.defaultConfigVector 
                        (SourceFile filePath) str

        -- Concretize rate type variables in a Disciple Core Flow program.
        ModeFlowConcretize filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdFlowConcretize dconfig (SourceFile filePath) str

        -- Melt compound data structures.
        ModeFlowMelt filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdFlowMelt dconfig (SourceFile filePath) str

        -- Wind loop primops into tail recursive loops.
        ModeFlowWind filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdFlowWind dconfig (SourceFile filePath) str

        -- Thread the world token through a Disciple Core Flow program.
        ModeFlowThread filePath
         -> do  dconfig <- getDriverConfig config (Just filePath)
                str     <- readFile filePath
                runError $ cmdFlowThread dconfig (SourceFile filePath) str

        -- Build --------------------------------------------------------------
        -- Build the runtime and base libraries.
        ModeBaseBuild
         -> do  dconfig <- getDriverConfig config Nothing
                store   <- Store.new
                runError $ cmdBaseBuild dconfig store

        -- Print --------------------------------------------------------------
        -- Print the external builder info for this platform.
        ModePrintBuilder
         -> do  dconfig <- getDriverConfig config Nothing
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
             { Runtime.configHeapSize             = configRuntimeHeapSize config }

        let dconfig
             = Driver.Config
             { Driver.configLogBuild              = True
             , Driver.configDump                  = configDump config
             , Driver.configInferTypes            = configInferTypes config
             , Driver.configSimplSalt             = Simplifier.idsimp
             , Driver.configViaBackend            = configViaBackend config
             , Driver.configRuntime               = runtimeConfig
             , Driver.configBuilder               = builder
             , Driver.configPretty                = Driver.defaultConfigPretty
             , Driver.configSuppressHashImports   = False
             , Driver.configModuleBaseDirectories = []
             , Driver.configOutputFile            = configOutputFile config
             , Driver.configOutputDir             = configOutputDir  config 
             , Driver.configKeepLlvmFiles         = configKeepLlvmFiles config
             , Driver.configKeepSeaFiles          = configKeepSeaFiles  config
             , Driver.configKeepAsmFiles          = configKeepAsmFiles  config 
             , Driver.configTaintAvoidTypeChecks  = configTaintAvoidTypeChecks config
             , Driver.configRuntimeLinkStrategy   = configRuntimeLinkStrategy config }
        
        -- We need to force -infer on because the inliner templates may not
        -- have full type annotations.
        simplSalt <- getSimplSaltOfConfig config 
                        dconfig { Driver.configInferTypes = True }
                        builder runtimeConfig filePath

        return  $ dconfig        
                { Driver.configSimplSalt        = simplSalt }


-- | Print errors to stderr and set the exit code.
runError :: ExceptT String IO a -> IO ()
runError m
 = do   result  <- runExceptT m
        case result of
         Left err       
          -> do hPutStrLn stderr err
                exitWith $ ExitFailure 1

         Right _
          -> return ()

