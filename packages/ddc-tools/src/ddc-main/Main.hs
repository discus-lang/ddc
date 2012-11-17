
-- | Unix-style command line interface to DDC.
--    This interface exports commands that work on whole modules at a time,
--    and does unix-style command line argument parsing.
--
import DDC.Main.Config
import DDC.Main.Args
import DDC.Main.OptLevels
import DDC.Driver.Command.Load
import DDC.Driver.Command.Compile
import DDC.Driver.Command.Make
import DDC.Driver.Command.Ast
import DDC.Driver.Command.ToSalt
import DDC.Driver.Command.ToC
import DDC.Driver.Command.ToLlvm
import DDC.Driver.Source
import DDC.Build.Builder
import DDC.Base.Pretty
import System.Environment
import System.IO
import System.Exit
import Control.Monad.Trans.Error
import qualified DDC.Driver.Stage       as Driver
import qualified DDC.Core.Salt.Runtime  as Runtime


main
 = do   args    <- getArgs
        config  <- parseArgs args defaultConfig
        run config


run config
 = case configMode config of
        -- We didn't get any arguments on the command line.
        ModeNone
         -> do  putStr help
                return ()

        -- Display the help page.
        ModeHelp
         -> do  putStr help
                return ()

        -- Just load, parse and type check a module.
        ModeLoad filePath
         -> do  let Just bundle = bundleFromFilePath config filePath
                str        <- readFile filePath
                cmdLoad bundle
                        (SourceFile filePath)
                        str

        -- Compile a module to object code.
        ModeCompile filePath
         -> do  dconfig  <- getDriverConfig config
                runError $ cmdCompile dconfig filePath

        -- Compile a module into an executable.
        ModeMake filePath
         -> do  dconfig  <- getDriverConfig config
                runError $ cmdMake    dconfig filePath

        -- Pretty print the AST of a module.
        ModeAST filePath
         -> do  let Just bundle = bundleFromFilePath config filePath
                str        <- readFile filePath
                cmdAstModule 
                        bundle
                        (SourceFile filePath) 
                        str

        -- Convert a module to Salt.
        ModeToSalt filePath
         -> do  let Just bundle = bundleFromFilePath config filePath
                dconfig         <- getDriverConfig config
                str             <- readFile filePath
                runError $ cmdToSalt dconfig bundle (SourceFile filePath) str

        -- Convert a module to C
        ModeToC filePath
         -> do  let Just bundle = bundleFromFilePath config filePath
                dconfig         <- getDriverConfig config
                str             <- readFile filePath
                runError $ cmdToC    dconfig bundle (SourceFile filePath) str

        -- Convert a module to LLVM
        ModeToLLVM filePath
         -> do  let Just bundle = bundleFromFilePath config filePath
                dconfig         <- getDriverConfig config
                str             <- readFile filePath
                runError $ cmdToLlvm dconfig bundle (SourceFile filePath) str

        -- Print the external builder info for this platform.
        ModePrintBuilder
         -> do  dconfig         <- getDriverConfig config
                putStrLn $ renderIndent $ ppr (Driver.configBuilder dconfig)


-- | Get the compile driver from the config.
getDriverConfig :: Config -> IO Driver.Config
getDriverConfig config
 = do   Just builder    <- determineDefaultBuilder (defaultBuilderConfig config)
        simplLite       <- getSimplLiteOfConfig config builder
        simplSalt       <- getSimplSaltOfConfig config builder

        return  $ Driver.Config
                { Driver.configDump                     = configDump config
                , Driver.configSimplLite                = simplLite
                , Driver.configSimplSalt                = simplSalt
                , Driver.configViaBackend               = configViaBackend config

                , Driver.configRuntime               
                        = Runtime.Config
                        { Runtime.configHeapSize        = configRuntimeHeapSize config }

                , Driver.configBuilder                  = builder
                , Driver.configSuppressCoreImports      = False
                , Driver.configSuppressHashImports      = False
                , Driver.configOutputFile               = configOutputFile config
                , Driver.configOutputDir                = configOutputDir  config }


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

