
module DDCI.Tetra.Command.Parse
        (cmdParse)
where
import DDC.Interface.Source
import DDCI.Tetra.State
import DDC.Base.Pretty
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty                  ()
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Base.Parser                as BP


cmdParse :: State -> Source -> String -> IO ()
cmdParse _state source str
 = goLex
 where  goLex 
         = let  tokens  = lexModuleString (nameOfSource source) 1 str
           in   goParse tokens

        goParse tokens
         = let  context = Context
                        { contextTrackedEffects         = True
                        , contextTrackedClosures        = True
                        , contextFunctionalEffects      = False
                        , contextFunctionalClosures     = False }

           in   case BP.runTokenParser 
                        C.describeTok 
                        (nameOfSource source)
                        (pModule context)
                        tokens of
                 Left err        -> error $ show err
                 Right mm        
                  -> do putStrLn (renderIndent $ ppr mm)
