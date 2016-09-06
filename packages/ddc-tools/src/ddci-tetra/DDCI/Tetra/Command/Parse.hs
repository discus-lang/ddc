
module DDCI.Tetra.Command.Parse
        (cmdParse)
where
import DDCI.Tetra.State
import DDC.Driver.Interface.Source
import DDC.Data.Pretty
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty          ()
import qualified DDC.Core.Lexer         as C
import qualified DDC.Control.Parser        as BP


cmdParse :: State -> Source -> String -> IO ()
cmdParse _state source str
 = goLex
 where  goLex 
         = let  tokens  = lexModuleString (nameOfSource source) 1 str
           in   goParse tokens

        goParse tokens
         = case BP.runTokenParser 
                        C.describeToken (nameOfSource source)
                        pModule tokens of
                 Left err        -> error $ show err
                 Right mm        
                  -> do putStrLn (renderIndent $ ppr mm)
