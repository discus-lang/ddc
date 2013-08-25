
module DDCI.Tetra.Command.ToCore
        (cmdToCore)
where
import DDC.Interface.Source
import DDCI.Tetra.State
import DDC.Base.Pretty
import DDC.Source.Tetra.Env
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty          ()
import DDC.Source.Tetra.Desugar.Defix
import DDC.Source.Tetra.Infer.Expand    as Expand
import DDC.Source.Tetra.ToCore          as ToCore
import qualified DDC.Core.Lexer         as C
import qualified DDC.Base.Parser        as BP
import qualified DDC.Data.SourcePos     as SP


cmdToCore :: State -> Source -> String -> IO ()
cmdToCore _state source str
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
         = do   let mm' = Expand.expand Expand.configDefault 
                                primKindEnv primTypeEnv mm
                goToCore mm'

        goToCore mm
         = do   let sp  = SP.SourcePos "<top level>" 1 1
                let mm' = ToCore.toCoreModule sp mm
                putStrLn (renderIndent $ ppr mm')

