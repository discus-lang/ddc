
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
        | "-ast" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeAST file }

        | "-load" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeLoad file }

        | "-make" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeMake file }

        | compile : file : rest <- args
        , elem compile ["-c", "-compile"]
        = parseArgs rest
        $ config { configMode   = ModeCompile file}

        | otherwise
        = error $ "Cannot parse arguments " ++ show args


help :: String
help    = unlines
        [ "The Disciplined Disciple Compiler, version 0.3.0"
        , "       -help             Display this help."
        , "       -ast     <file>   Pretty print the AST of a module."
        , "       -load    <file>   Parse and type-check a module."
        , "  -c,  -compile <file>   Compile a module into an object file."
        , "       -make    <file>   Compile a module into an executable file."
        , "" ]

