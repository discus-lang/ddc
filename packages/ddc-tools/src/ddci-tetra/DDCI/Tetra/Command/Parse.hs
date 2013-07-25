
module DDCI.Tetra.Command.Parse
        (cmdParse)
where
import DDC.Interface.Source
import DDCI.Tetra.State
import qualified DDC.Core.Lexer as C


cmdParse :: State -> Source -> String -> IO ()
cmdParse _state _source str
 = do   let sourcePathName = "<interactive>"
        let tokens  = C.lexModuleWithOffside sourcePathName 1 str
        putStrLn $ show tokens
        return ()

