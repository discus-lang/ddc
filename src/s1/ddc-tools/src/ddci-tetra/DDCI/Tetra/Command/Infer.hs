{-# LANGUAGE TypeFamilies #-}
module DDCI.Tetra.Command.Infer
        (cmdInfer)
where
import DDCI.Tetra.State
import DDC.Driver.Interface.Source
import DDC.Data.Pretty
import DDC.Source.Discus.Lexer
import DDC.Source.Discus.Parser
import DDC.Source.Discus.Pretty                  ()
import DDC.Source.Discus.Transform.Defix
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Control.Parser             as BP


cmdInfer :: State -> Source -> String -> IO ()
cmdInfer _state source str
 = goLex
 where  goLex
         = let  tokens  = lexModuleString (nameOfSource source) 1 str
           in   goParse tokens

        goParse tokens
         = case BP.runTokenParser C.describeToken (nameOfSource source) pModule tokens of
                 Left err        -> error $ show err
                 Right mm        -> goDesugar mm

        goDesugar mm
         = case defix defaultFixTable mm of
            Left err    -> putStrLn (renderIndent $ ppr err)
            Right mm'   -> goExpand mm'

        goExpand mm
         = do   putStrLn (renderIndent $ ppr mm)

