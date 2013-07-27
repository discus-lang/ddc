
module DDCI.Tetra.Command.Parse
        (cmdParse)
where
import DDC.Interface.Source
import DDCI.Tetra.State
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Source.Tetra.Parser        as ST
import qualified DDC.Base.Parser                as BP


cmdParse :: State -> Source -> String -> IO ()
cmdParse _state source str
 = goLex
 where  goLex 
         = let  tokens  = C.lexModuleWithOffside (nameOfSource source) 1 str
           in   goParse tokens

        goParse tokens
         = let  context = ST.Context
                        { ST.contextTrackedEffects         = True
                        , ST.contextTrackedClosures        = True
                        , ST.contextFunctionalEffects      = False
                        , ST.contextFunctionalClosures     = False }

           in   case BP.runTokenParser 
                        C.describeTok 
                        (nameOfSource source)
                        (ST.pModule context)
                        tokens of
                 Left err        -> error $ show err
                 Right mm        
                  -> do putStrLn (show mm)
