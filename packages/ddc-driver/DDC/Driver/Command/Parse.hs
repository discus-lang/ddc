
module DDC.Driver.Command.Parse
        (cmdParseModule)
where
import DDC.Driver.Source
import DDC.Driver.Stage
import Control.Monad.Trans.Error
import System.FilePath


cmdParseModule :: Config -> Source -> String -> ErrorT String IO ()
cmdParseModule config source str
 | SourceFile filePath  <- source
 = case takeExtension filePath of
        ".dst"  -> cmdParseModule_tetra config source str
        _       -> throwError "Unknown file extension."

 | otherwise
 = throwError "Cannot detect language."


cmdParseModule_tetra _config _source _str
 = do   error "blerk"
