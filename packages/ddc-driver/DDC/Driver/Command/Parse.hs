
module DDC.Driver.Command.Parse
        ( cmdParseFromFile
        , cmdParseSourceTetraFromFile)
where
import DDC.Driver.Stage
import DDC.Source.Tetra.Pretty
import DDC.Source.Tetra.Lexer           as ST
import DDC.Source.Tetra.Parser          as ST
import DDC.Core.Lexer                   as C
import DDC.Base.Parser                  as BP
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
 
 -- Parse a Disciple Source Tetra module
 | ".dst"       <- takeExtension filePath
 =  cmdParseSourceTetraFromFile config filePath

 | otherwise
 = let  ext     = takeExtension filePath
   in   throwError $ "Cannot parse '" ++ ext ++ "' files."


-------------------------------------------------------------------------------
-- | Parse a Disciple Source Tetra module from a file.
--   The result AST is printed to @stdout@.
--   Any errors are thrown in the `ErrorT` monad.
cmdParseSourceTetraFromFile
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file name.
        -> ErrorT String IO ()

cmdParseSourceTetraFromFile _config filePath
 = do   
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwError $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        -- Lex the source string.
        let toks    = ST.lexModuleString filePath 1 src

        let context = ST.Context
                    { ST.contextTrackedEffects          = True
                    , ST.contextTrackedClosures         = True
                    , ST.contextFunctionalEffects       = False
                    , ST.contextFunctionalClosures      = False }
                    
        case BP.runTokenParser
                C.describeTok filePath (pModule context) toks of
         Left err 
          ->    throwError (renderIndent $ ppr err)
         
         Right mm
          ->    liftIO $ putStrLn (renderIndent $ ppr mm)


