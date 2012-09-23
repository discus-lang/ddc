
-- | Parsing of command line configuation arguments.
module DDC.Main.Args 
        ( parseArgs
        , help)
where
import DDC.Main.Config
import Data.Char

-- | Parse command line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
        = return config

parseArgs args config
        -- General ------------------------------
        | flag : _              <- args
        , elem flag ["-h", "-help", "--help"]
        = return 
        $ config { configMode   = ModeHelp }

        | "-make" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeMake file }

        -- Compilation --------------------------
        | compile : file : rest <- args
        , elem compile ["-c", "-compile"]
        = parseArgs rest
        $ config { configMode   = ModeCompile file}

        | "-library" : path : rest <- args
        = parseArgs rest
        $ config { configLibraryPath = path }

        | flag : file : rest     <- args
        , elem flag    ["-o", "-output"]
        = parseArgs rest
        $ config { configOutputFile = Just file }

        | "-fvia-c" : rest      <- args
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

        -- Runtime ------------------------------
        | "-run-heap" : bytes : rest    <- args
        , all isDigit bytes
        = parseArgs rest
        $ config  { configRuntimeHeapSize = read bytes }

        -- Conversion ---------------------------
        | "-to-salt" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeToSalt file }

        | "-to-c" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeToC file }

        | "-to-llvm" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeToLLVM file }

        -- Debugging ----------------------------
        | "-dump" : rest        <- args
        = parseArgs rest
        $ config { configDump   = True }

        | "-load" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeLoad file }

        | "-ast" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeAST file }

        | otherwise
        = error $ "Cannot parse arguments " ++ show args


help :: String
help    = unlines
        [ "The Disciplined Disciple Compiler, version 0.3.0"
        , ""
        , " General:"
        , "       -help                Display this help."
        , ""
        , " Compilation:"
        , "       -make       <file>   Compile a module into an executable file."
        , "  -c,  -compile    <file>   Compile a module into an object file."
        , ""
        , "       -library    <dir>    Path to the base library code (./code)"
        , ""
        , "       -fvia-llvm           Compile via the LLVM backend  (default)"
        , "       -fvia-c              Compile via the C backend."
        , ""
        , "  -o,  -output     <file>   Redirect output to this file."
        , "       -output-dir <dir>    Redirect output to this directory."
        , ""
        , " Optimisation:"
        , "       -O0                  No optimisations.             (default)"
        , "  -O,  -O1                  Do standard optimisations."
        , ""
        , " Runtime for compiled program:"
        , "       -run-heap   <bytes>  Size of fixed heap            (65536)"
        , ""
        , " Conversion:"
        , "       -to-salt    <file>   Convert a module to Disciple Core Salt."
        , "       -to-c       <file>   Convert a module to C code."
        , "       -to-llvm    <file>   Convert a module to LLVM code."
        , ""
        , " Debugging:"
        , "       -dump                Dump intermediate representations."
        , "       -load       <file>   Parse and type-check a module."
        , "       -ast        <file>   Pretty print the AST of a module."
        , "" ]

