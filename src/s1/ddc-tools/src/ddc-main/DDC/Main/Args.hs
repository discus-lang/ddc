
-- In GHC 8 we need to turn of the pattern match checker because the new algorithm
-- runs out of stack space when checking this module.
-- https://ghc.haskell.org/trac/ghc/ticket/11822
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}

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

        -- Language -----------------------------
        | "-infer" : rest           <- args
        = parseArgs rest
        $ config { configInferTypes = True }

        | flag : rest           <- args
        , elem flag ["-recon", "-no-infer"]
        = parseArgs rest
        $ config { configInferTypes = False }

        -- Compilation --------------------------
        | flag : file : rest <- args
        , elem flag ["-c", "-compile", "--compile"]
        = parseArgs rest
        $ setMode config $ ModeCompile file

        | flag : file : rest    <- args
        , elem flag ["-make", "--make" ]
        = parseArgs rest
        $ setMode config $ ModeMake file

        | flag : file : rest    <- args
        , elem flag ["-build", "--build"]
        = parseArgs rest
        $ setMode config $ ModeBuild file

        | "-basedir" : path : rest <- args
        = parseArgs rest
        $ config { configBaseDir = path }

        | "-basebuild" : rest   <- args
        = parseArgs rest
        $ setMode config $ ModeBaseBuild

        | "-fvia-c"    : rest      <- args
        = parseArgs rest
        $ config { configViaBackend = ViaC }

        | "-fvia-llvm" : rest   <- args
        = parseArgs rest
        $ config { configViaBackend = ViaLLVM }

        | flag         : file : rest     <- args
        , elem flag    ["-o", "-output"]
        = parseArgs rest
        $ config { configOutputFile = Just file }

        | flag : dir : rest     <- args
        , elem flag    ["-output-dir"]
        = parseArgs rest
        $ config { configOutputDir  = Just dir }

        -- Optimisation -------------------------
        | "-O0" : rest          <- args
        = parseArgs rest
        $ config { configOptLevelSalt   = OptLevel0 }

        | flag : rest          <- args
        , elem flag     [ "-O", "-O1" ]
        = parseArgs rest
        $ config { configOptLevelSalt   = OptLevel1 }

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
        $ config { configRuntimeHeapSize = read bytes }

        | "-run-link-static" : rest    <- args
        = parseArgs rest
        $ config { configRuntimeLinkStrategy = LinkStatic }

        -- Parsing ------------------------------
        | "-scan"  : file : rest        <- args
        = parseArgs rest
        $ config { configMode   = ModeScan  file True }

        | "-scan-no-locations" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeScan  file False }

        | "-parse" : file : rest        <- args
        = parseArgs rest
        $ config { configMode   = ModeParse file }

        -- Checking -----------------------------
        | "-check"  : file : rest       <- args
        = parseArgs rest
        $ config { configMode   = ModeCheck file }

        -- Transformation -----------------------
        | "-load"   : file : rest       <- args
        = parseArgs rest
        $ setMode config $ ModeLoad file

        | "-trans" : trans : rest       <- args
        = parseArgs rest
        $ config { configTrans = Just trans }

        | "-with"  : file  : rest       <- args
        = parseArgs rest
        $ config { configWith  = configWith config ++ [file] }

        -- Flow ---------------------------------
        | "-flow-prep" : file : rest    <- args
        = parseArgs rest
        $ setMode config $ ModeFlowPrep file

        | "-flow-lower" : file : rest   <- args
        = parseArgs rest
        $ setMode config $ ModeFlowLower file

        | "-flow-lower-kernel" : file : rest   <- args
        = parseArgs rest
        $ setMode config $ ModeFlowLowerKernel file

        | "-flow-lower-vector" : file : rest   <- args
        = parseArgs rest
        $ setMode config $ ModeFlowLowerVector file

        | "-flow-concretize" : file : rest <- args
        = parseArgs rest
        $ setMode config $ ModeFlowConcretize file

        | "-flow-melt" : file : rest <- args
        = parseArgs rest
        $ setMode config $ ModeFlowMelt file

        | "-flow-wind" : file : rest    <- args
        = parseArgs rest
        $ setMode config $ ModeFlowWind file

        | "-flow-thread" : file : rest  <- args
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

        | "-to-php"  : file : rest  <- args
        = parseArgs rest
        $ setMode config $ ModeToPHP file

        -- Debugging ----------------------------
        | "-dump"   : rest        <- args
        = parseArgs rest
        $ config { configDump   = True }

        | "-print-builder" : rest <- args
        = parseArgs rest
        $ setMode config ModePrintBuilder

        | "-print-basedir" : rest <- args
        = parseArgs rest
        $ setMode config ModePrintBaseDir

        -- Taints -------------------------------
        | "-taint-avoid-type-checks" : rest     <- args
        = parseArgs rest
        $ config { configTaintAvoidTypeChecks = True }

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
     -> error "ddc-main.setMode: not setting mode to ModeNone"

    Just newFlag
     -> case flagOfMode $ configMode config of
         Nothing        -> config { configMode = newMode }
         Just oldFlag   -> error $ "Cannot use " ++ newFlag ++ " with " ++ oldFlag


-- | Get the flag used to set DDC to this mode.
flagOfMode :: Mode -> Maybe String
flagOfMode mode
 = case mode of
        ModeNone{}                      -> Nothing
        ModeVersion{}                   -> Just "-version"
        ModeHelp{}                      -> Just "-help"
        ModeParse{}                     -> Just "-parse"
        ModeScan _ True                 -> Just "-scan"
        ModeScan _ False                -> Just "-scan-no-locations"
        ModeCheck{}                     -> Just "-check"
        ModeLoad{}                      -> Just "-load"
        ModeCompile{}                   -> Just "-compile"
        ModeMake{}                      -> Just "-make"
        ModeBuild{}                     -> Just "-build"
        ModeToSalt{}                    -> Just "-to-salt"
        ModeToC{}                       -> Just "-to-c"
        ModeToLLVM{}                    -> Just "-to-llvm"
        ModeToPHP{}                     -> Just "-to-php"
        ModeFlowPrep{}                  -> Just "-flow-prep"
        ModeFlowLower{}                 -> Just "-flow-lower"
        ModeFlowLowerKernel{}           -> Just "-flow-lower-kernel"
        ModeFlowLowerVector{}           -> Just "-flow-lower-vector"
        ModeFlowConcretize{}            -> Just "-flow-concretize"
        ModeFlowMelt{}                  -> Just "-flow-melt"
        ModeFlowThread{}                -> Just "-flow-thread"
        ModeFlowWind{}                  -> Just "-flow-wind"
        ModeBaseBuild{}                 -> Just "-basebuild"
        ModePrintBuilder{}              -> Just "-print-builder"
        ModePrintBaseDir{}              -> Just "-print-basedir"


