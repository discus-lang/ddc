
module DDC.Driver.Command.Parse
        ( cmdParseFromFile
        , cmdParseSourceTetraFromFile
        , cmdParseCoreFromFile)
where
import DDC.Driver.Stage
import DDC.Build.Language
import DDC.Source.Discus.Pretty
import DDC.Source.Discus.Lexer          as ST
import DDC.Source.Discus.Parser         as ST
import DDC.Core.Fragment                as C
import DDC.Core.Parser                  as C
import DDC.Core.Lexer                   as C
import DDC.Control.Parser               as BP
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import Control.Monad
import qualified DDC.Data.SourcePos     as SP


-------------------------------------------------------------------------------
-- | Parse a module.
--   The result AST is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
--
--   This function handle fragments of Disciple Core, as well as Source Tetra
--   modules. The language to use is determined by inspecting the file name
--   extension.
--
cmdParseFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file name.
        -> ExceptT String IO ()

cmdParseFromFile config filePath

 -- Parse a Disciple Source Tetra module.
 | ".ds"         <- takeExtension filePath
 =  cmdParseSourceTetraFromFile config filePath

 -- Parse a module in some fragment of Disciple Core.
 | Just language <- languageOfExtension (takeExtension filePath)
 = cmdParseCoreFromFile config language filePath

 -- Don't know how to parse this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwE $ "Cannot parse '" ++ ext ++ "' files."


-------------------------------------------------------------------------------
-- | Parse a Disciple Source Tetra module from a file.
--   The result AST is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdParseSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdParseSourceTetraFromFile config filePath
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        -- Lex the source string.
        let toks    = ST.lexModuleString filePath 1 src

        when (configDump config)
         $ liftIO $ writeFile "dump.tetra-parse.tokens-sp"
                  $ unlines $ map show toks

        when (configDump config)
         $ liftIO $ writeFile "dump.tetra-parse.tokens"
                  $ unlines $ map show $ map SP.valueOfLocated toks

        case BP.runTokenParser
                C.describeToken filePath ST.pModule toks of
         Left err
          ->    throwE (renderIndent $ ppr err)

         Right mm
          ->    liftIO $ putStrLn (renderIndent $ ppr mm)


-------------------------------------------------------------------------------
-- | Parse a Disciple Core module from a file.
--   The AST is printed to @stdout@.
--   Any errors are thrown in the `ExceptT` monad.
cmdParseCoreFromFile
        :: Config               -- ^ Driver config
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ExceptT String IO ()

cmdParseCoreFromFile _config language filePath
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        -- Lex the source string.
        let toks = (C.fragmentLexModule fragment) filePath 1 src

        case BP.runTokenParser
                C.describeToken filePath
                (C.pModule (C.contextOfProfile profile)) toks of
         Left err
          ->    throwE (renderIndent $ ppr err)

         Right mm
          ->    liftIO $ putStrLn (renderIndent $ ppr mm)
