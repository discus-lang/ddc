
module DDC.Main.Args 
        ( parseArgs
        , help)
where
import DDC.Main.Config


-- | Parse command line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
        = return config

parseArgs args config
        | flag : _              <- args
        , elem flag ["-h", "-help", "--help"]
        = return 
        $ config { configMode   = ModeHelp }

        | "-make" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeMake file }

        | compile : file : rest <- args
        , elem compile ["-c", "-compile"]
        = parseArgs rest
        $ config { configMode   = ModeCompile file}

        | "-load" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeLoad file }

        | "-ast" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeAST file }

        | "-to-salt" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeToSalt file }

        | "-to-c" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeToC file }

        | "-to-llvm" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeToLLVM file }

        | otherwise
        = error $ "Cannot parse arguments " ++ show args


help :: String
help    = unlines
        [ "The Disciplined Disciple Compiler, version 0.3.0"
        , ""
        , "       -help             Display this help."
        , ""
        , "  -c,  -compile <file>   Compile a module into an object file."
        , "       -make    <file>   Compile a module into an executable file."
        , ""
        , "       -load    <file>   Parse and type-check a module."
        , "       -ast     <file>   Pretty print the AST of a module."
        , ""
        , "       -to-salt <file>   Convert a module to Disciple Core Salt."
        , "       -to-c    <file>   Convert a module to C code."
        , "       -to-llvm <file>   Convert a module to LLVM code."
        , "" ]

