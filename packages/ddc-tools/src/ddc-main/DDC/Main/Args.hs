
-- | Parsing of command line configuation arguments.
module DDC.Main.Args 
        ( parseArgs
        , help)
where
import DDC.Main.Config
import DDC.Main.Help
import Data.Char


-- | Parse command line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
        = return config

parseArgs args config
        -- General ------------------------------
        | flag : _              <- args
        , elem flag ["-version", "--version"]
        = return
        $ config { configMode   = ModeVersion }

        | flag : _              <- args
        , elem flag ["-h", "-help", "--help"]
        = return 
        $ config { configMode   = ModeHelp }

        | flag : file : rest    <- args
        , elem flag ["-make", "--make" ]
        = parseArgs rest
        $ setMode config $ ModeMake file

        | "-basebuild" : rest   <- args
        = parseArgs rest
        $ setMode config $ ModeBaseBuild

        -- Compilation --------------------------
        | flag : file : rest <- args
        , elem flag ["-c", "-compile", "--compile"]
        = parseArgs rest
        $ setMode config $ ModeCompile file

        | "-basedir" : path : rest <- args
        = parseArgs rest
        $ config { configBaseDir = path }

        | flag         : file : rest     <- args
        , elem flag    ["-o", "-output"]
        = parseArgs rest
        $ config { configOutputFile = Just file }

        | "-fvia-c"    : rest      <- args
        = parseArgs rest
        $ config { configViaBackend = ViaC }

        | "-fvia-llvm" : rest   <- args
        = parseArgs rest
        $ config { configViaBackend = ViaLLVM }

        | flag : dir : rest     <- args
        , elem flag    ["-output-dir"]
        = parseArgs rest
        $ config { configOutputDir  = Just dir }

        -- Optimisation -------------------------
        | "-O0" : rest          <- args
        = parseArgs rest
        $ config { configOptLevelLite   = OptLevel0 
                 , configOptLevelSalt   = OptLevel0 }

        | flag : rest          <- args
        , elem flag     [ "-O", "-O1" ]
        = parseArgs rest
        $ config { configOptLevelLite   = OptLevel1
                 , configOptLevelSalt   = OptLevel1 }

        -- Intermediates ------------------------
        | flag : rest   <- args
        , elem flag ["-keep-ll-files", "-keep-llvm-files" ]
        = parseArgs rest
        $ config { configKeepLlvmFiles = True }

        | flag : rest      <- args
        , elem flag ["-keep-c-files",  "-keep-sea-files" ]
        = parseArgs rest
        $ config { configKeepSeaFiles = True }

        | flag : rest      <- args
        , elem flag ["-keep-s-files", "-keep-asm-files" ]
        = parseArgs rest
        $ config { configKeepAsmFiles = True }

        -- Runtime ------------------------------
        | "-run-heap" : bytes : rest    <- args
        , all isDigit bytes
        = parseArgs rest
        $ config  { configRuntimeHeapSize = read bytes }

        -- Checking -----------------------------
        | "-check" : file : rest    <- args
        = parseArgs rest
        $ config { configMode  = ModeCheck file }

        -- Transformation -----------------------
        | "-load"   : file : rest <- args
        = parseArgs rest
        $ setMode config $ ModeLoad file

        | "-trans" : trans : rest   <- args
        = parseArgs rest
        $ config { configTrans = Just trans }

        | "-with"  : file  : rest   <- args
        = parseArgs rest
        $ config { configWith  = configWith config ++ [file] }

        -- Flow ---------------------------------
        | "-flow-prep" : file : rest <- args
        = parseArgs rest
        $ setMode config $ ModeFlowPrep file

        | "-flow-lower" : file : rest <- args
        = parseArgs rest
        $ setMode config $ ModeFlowLower file

        | "-flow-concretize" : file : rest <- args
        = parseArgs rest
        $ setMode config $ ModeFlowConcretize file

        | "-flow-thread" : file : rest <- args
        = parseArgs rest
        $ setMode config $ ModeFlowThread file

        -- Conversion ---------------------------
        | "-to-salt" : file : rest  <- args
        = parseArgs rest
        $ setMode config $ ModeToSalt file

        | "-to-c"    : file : rest  <- args
        = parseArgs rest
        $ setMode config $ ModeToC file

        | "-to-llvm" : file : rest  <- args
        = parseArgs rest
        $ setMode config $ ModeToLLVM file

        -- Debugging ----------------------------
        | "-dump"   : rest        <- args
        = parseArgs rest
        $ config { configDump   = True }

        | "-ast"   : file : rest  <- args
        = parseArgs rest
        $ setMode config $ ModeAST file

        | "-print-builder" : rest <- args
        = parseArgs rest
        $ setMode config ModePrintBuilder

        | "-print-basedir" : rest <- args
        = parseArgs rest
        $ setMode config ModePrintBaseDir

        -- If we get some other argument starting with '-' then assume it's
        -- a flag we don't support.
        | arg : _               <- args
        , '-' : _               <- arg
        = error $ "Cannot parse arguments " ++ show args

        -- Otherwise, treat the argument as a source file to make.
        | arg : rest            <- args
        = parseArgs rest
        $ setMode config (ModeMake arg)

        | otherwise
        = error $ "Cannot parse arguments " ++ show args


-- | Set the major mode of DDC.
-- 
--   We can't have two major modes like '-make' and '-compile' set at the same time.
--   If this happens then `error`.
setMode :: Config -> Mode -> Config
setMode config newMode
 | ModeMake{}    <- configMode config
 , ModeMake{}    <- newMode
 = error "Multi-module compilation is not supported yet."

 | ModeCompile{} <- configMode config
 , ModeCompile{} <- newMode
 = error "Multi-module compilation is not supported yet."

 | otherwise
 = case flagOfMode newMode of
    Nothing     
     -> error "DDC.Main.Args.setMode: not setting mode to ModeNone"

    Just newFlag
     -> case flagOfMode $ configMode config of
         Nothing        -> config { configMode = newMode }
         Just oldFlag   -> error $ "Cannot use " ++ newFlag ++ " with " ++ oldFlag


-- | Get the flag used to set DDC to this mode.
flagOfMode :: Mode -> Maybe String
flagOfMode mode
 = case mode of
        ModeNone{}              -> Nothing
        ModeVersion{}           -> Just "-version"
        ModeHelp{}              -> Just "-help"
        ModeCheck{}             -> Just "-check"
        ModeLoad{}              -> Just "-load"
        ModeCompile{}           -> Just "-compile"
        ModeMake{}              -> Just "-make"
        ModeAST{}               -> Just "-ast"
        ModeToSalt{}            -> Just "-to-salt"
        ModeToC{}               -> Just "-to-c"
        ModeToLLVM{}            -> Just "-to-llvm"
        ModeFlowPrep{}          -> Just "-flow-prep"
        ModeFlowLower{}         -> Just "-flow-lower"
        ModeFlowConcretize{}    -> Just "-flow-concretize"
        ModeFlowThread{}        -> Just "-flow-thread"
        ModeBaseBuild{}         -> Just "-basebuild"
        ModePrintBuilder{}      -> Just "-print-builder"
        ModePrintBaseDir{}      -> Just "-print-basedir"


