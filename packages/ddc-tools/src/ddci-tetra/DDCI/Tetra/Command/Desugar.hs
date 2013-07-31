
module DDCI.Tetra.Command.Desugar
        (cmdDesugar)
where
import DDC.Interface.Source
import DDCI.Tetra.State
import DDC.Base.Pretty
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Name
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty          ()
import DDC.Source.Tetra.Desugar.Defix
import qualified DDC.Core.Lexer         as C
import qualified DDC.Base.Parser        as BP


infixTable :: InfixTable BP.SourcePos Name
infixTable
 = InfixTable []


cmdDesugar :: State -> Source -> String -> IO ()
cmdDesugar _state source str
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
                 Right mm        -> goDesugar mm

        goDesugar mm
         = case defixModule infixTable mm of
                Left err        -> error $ show err
                Right mm'
                 -> putStrLn (renderIndent $ ppr mm')
