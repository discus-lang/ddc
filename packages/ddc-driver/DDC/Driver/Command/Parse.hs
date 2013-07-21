
module DDC.Driver.Command.Parse
        (cmdParseModule)
where
import DDC.Driver.Source
import DDC.Driver.Stage
import Control.Monad.Trans.Error
import System.FilePath
import qualified DDC.Core.Lexer as C


cmdParseModule :: Config -> Source -> String -> ErrorT String IO ()
cmdParseModule config source str
 | SourceFile filePath  <- source
 = case takeExtension filePath of
        ".dst"  -> cmdParseModule_tetra config filePath str
        _       -> throwError "Unknown file extension."

 | otherwise
 = throwError "Cannot detect language."


cmdParseModule_tetra _config sourcePathName str
 = do   let tokens = C.lexModuleWithOffside sourcePathName 1 str
        error $ show tokens
