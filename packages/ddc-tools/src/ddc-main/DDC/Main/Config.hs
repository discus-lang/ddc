
module DDC.Main.Config
        ( Mode   (..)
        , Config (..)
        , defaultConfig
        , parseArgs)
where

-- | The main command that we're running.
data Mode
        -- | Don't do anything
        = ModeNone

        -- | Pretty print a module's AST.
        | ModeAST FilePath
        deriving (Eq, Show)


-- | DDC driver config.
data Config
        = Config
        { configMode    :: Mode }
        deriving (Eq, Show)


-- | Default configuation.
defaultConfig :: Config
defaultConfig
        = Config
        { configMode    = ModeNone }


-- | Parse command line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
        = return config

parseArgs args config
        | "-ast" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeAST file }

        | otherwise
        = error "bad usage"


