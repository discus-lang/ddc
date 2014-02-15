
module DDC.Driver.Command.Parse
        ( cmdParseFromFile
        , cmdParseSourceTetraFromFile
        , cmdParseCoreFromFile)
where
import DDC.Driver.Stage
import DDC.Build.Language
import DDC.Source.Tetra.Pretty
import DDC.Source.Tetra.Lexer           as ST
import DDC.Source.Tetra.Parser          as ST
import DDC.Core.Fragment                as C
import DDC.Core.Parser                  as C
import DDC.Core.Lexer                   as C
import DDC.Base.Parser                  as BP
import DDC.Data.Token                   as Token
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import Control.Monad


-------------------------------------------------------------------------------
-- | Parse a module.
--   The result AST is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
--
--   This function handle fragments of Disciple Core, as well as Source Tetra
--   modules. The language to use is determined by inspecting the file name
--   extension.
--
cmdParseFromFile 
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file name.
        -> ErrorT String IO ()

cmdParseFromFile config filePath
 
 -- Parse a Disciple Source Tetra module.
 | ".dst"        <- takeExtension filePath
 =  cmdParseSourceTetraFromFile config filePath

 -- Parse a module in some fragment of Disciple Core.
 | Just language <- languageOfExtension (takeExtension filePath)
 = cmdParseCoreFromFile config language filePath

 -- Don't know how to parse this file.
 | otherwise
 = let  ext     = takeExtension filePath
   in   throwError $ "Cannot parse '" ++ ext ++ "' files."


-------------------------------------------------------------------------------
-- | Parse a Disciple Source Tetra module from a file.
--   The result AST is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
cmdParseSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()

cmdParseSourceTetraFromFile config filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        -- Lex the source string.
        let toks    = ST.lexModuleString filePath 1 src

        when (configDump config)
         $ liftIO $ writeFile "dump.tetra-parse.tokens-sp" 
                  $ unlines $ map show toks

        when (configDump config)
         $ liftIO $ writeFile "dump.tetra-parse.tokens" 
                  $ unlines $ map show $ map Token.tokenTok toks

        let context = ST.Context
                    { ST.contextTrackedEffects          = True
                    , ST.contextTrackedClosures         = True
                    , ST.contextFunctionalEffects       = False
                    , ST.contextFunctionalClosures      = False }
                    
        case BP.runTokenParser
                C.describeTok filePath 
                (ST.pModule context) toks of
         Left err 
          ->    throwError (renderIndent $ ppr err)
         
         Right mm
          ->    liftIO $ putStrLn (renderIndent $ ppr mm)


-------------------------------------------------------------------------------
-- | Parse a Disciple Core module from a file.
--   The AST is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
cmdParseCoreFromFile
        :: Config               -- ^ Driver config
        -> Language             -- ^ Core language definition.
        -> FilePath             -- ^ Module file path.
        -> ErrorT String IO ()

cmdParseCoreFromFile _config language filePath
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        -- Lex the source string.
        let toks = (C.fragmentLexModule fragment) filePath 1 src

        case BP.runTokenParser
                C.describeTok filePath 
                (C.pModule (C.contextOfProfile profile)) toks of
         Left err
          ->    throwError (renderIndent $ ppr err)

         Right mm
          ->    liftIO $ putStrLn (renderIndent $ ppr mm)
