
module Config where
import DDC.Build.Language
import qualified DDC.Build.Language.Lite        as Lite
import System.FilePath
import System.Exit


-- | DDC type checker command line interface config.
data Config
        = Config
        { -- | Source file to load, or Nothing to read source from stdin.
          configSourceFile      :: Maybe String

          -- | Language fragment to check.
        , configLanguage        :: Language 

          -- | If true then don't print checked source file to stdout.
        , configQuiet           :: Bool }


-- | Default command line configuration.
defaultConfig :: Config
defaultConfig
        = Config
        { configSourceFile      = Nothing
        , configLanguage        = Language Lite.bundle
        , configQuiet           = False }


-- | Parse command line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config     
        = return config

parseArgs args@(arg : more) config
        -- Display usage help.
        | arg == "-help" || arg == "--help"
        = do    putStr usage
                exitWith ExitSuccess

        -- Set quiet mode.
        | arg == "-quiet"
        = do    parseArgs more $ config { configQuiet   = True }

        -- Set the language fragment manually.
        | "-language" : lang : rest     <- args
        = case lookup lang languages of
           Just l  -> parseArgs rest $ config { configLanguage = l }
           Nothing -> error $ unlines
                        [ "unknown language " ++ lang
                        , "  options are: " ++ (show $ map fst languages) ]

        -- Try to guess the language fragment based on the file name extension.
        | fileName : [] <- args
        , extension     <- takeExtension fileName
        = case languageOfExtension extension of
           Just language
            -> return
            $  config { configSourceFile   = Just fileName
                      , configLanguage     = language }

           Nothing 
            ->  error $ "unrecognised extension " ++ show extension

        | otherwise
        = do    putStr usage
                error  $ "unknown argument " ++ show arg


-- | Command line usage information.
usage :: String
usage 
        = unlines
        [ "Disciplined Disciple Compiler type checker."
        , ""
        , " Usage:"
        , "  ddc-check [flags]           Read input from stdin."
        , "  ddc-check [flags] <file>    Read input from file."
        , ""
        , " Flags:"
        , "  -help                       Display this help."
        , "  -quiet                      Don't print checked module to stdout."
        , "  -language <fragment>        Set the language fragment.  (default Lite)"
        , "     fragment one of " ++ (show $ map fst languages)
        , "" ]

