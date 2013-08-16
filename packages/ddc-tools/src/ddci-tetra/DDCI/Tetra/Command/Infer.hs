
module DDCI.Tetra.Command.Infer
        (cmdInfer)
where
import DDC.Interface.Source
import DDCI.Tetra.State
import DDC.Base.Pretty
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty          ()
import DDC.Source.Tetra.Desugar.Defix
import DDC.Source.Tetra.Infer.Expand    as Expand
import qualified DDC.Core.Lexer         as C
import qualified DDC.Base.Parser        as BP


cmdInfer :: State -> Source -> String -> IO ()
cmdInfer _state source str
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

           in   case BP.runTokenParser C.describeTok (nameOfSource source)
                        (pModule context) tokens of
                 Left err        -> error $ show err
                 Right mm        -> goDesugar mm

        goDesugar mm
         = case defix defaultFixTable mm of
            Left err    -> putStrLn (renderIndent $ ppr err)
            Right mm'   -> goExpand mm'

        goExpand mm
         = do   let mm' = Expand.expandModule Expand.configDefault mm
                putStrLn (renderIndent $ ppr mm')

