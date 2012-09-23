
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
import System.Environment
import qualified DDC.Driver.Stage       as D


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
                cmdCompile dconfig filePath

        -- Compile a module into an executable.
        ModeMake filePath
         -> do  dconfig  <- getDriverConfig config
                cmdMake    dconfig filePath

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
                cmdToSalt dconfig bundle (SourceFile filePath) str

        -- Convert a module to C
        ModeToC filePath
         -> do  let Just bundle = bundleFromFilePath config filePath
                dconfig         <- getDriverConfig config
                str             <- readFile filePath
                cmdToC    dconfig bundle (SourceFile filePath) str

        -- Convert a module to LLVM
        ModeToLLVM filePath
         -> do  let Just bundle = bundleFromFilePath config filePath
                dconfig         <- getDriverConfig config
                str             <- readFile filePath
                cmdToLlvm dconfig bundle (SourceFile filePath) str


-- | Get the compile driver from the config.
getDriverConfig :: Config -> IO D.Config
getDriverConfig config
 = do   Just builder    <- determineDefaultBuilder defaultBuilderConfig
        simplLite       <- getSimplLiteOfConfig config builder
        simplSalt       <- getSimplSaltOfConfig config builder

        return  $ D.Config
                { D.configDump                  = configDump config
                , D.configSimplLite             = simplLite
                , D.configSimplSalt             = simplSalt
                , D.configBuilder               = builder
                , D.configSuppressCoreImports   = False
                , D.configSuppressHashImports   = False
                , D.configOutputFile            = configOutputFile config
                , D.configOutputDir             = configOutputDir  config }


