{-# LANGUAGE TypeFamilies #-}
module DDCI.Tetra.Command.Infer
        (cmdInfer)
where
import DDCI.Tetra.State
import DDC.Driver.Interface.Source
import DDC.Base.Pretty
-- import DDC.Source.Tetra.Env
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty                  ()
import DDC.Source.Tetra.Transform.Defix
-- import DDC.Source.Tetra.Transform.Expand        as Expand
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Base.Parser                as BP
-- import qualified DDC.Data.SourcePos             as SP


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
         = do   -- let sp            = SP.SourcePos "<top level>" 1 1

{-                let mm' = Expand.expand Expand.configDefault sp
                                primKindEnv primTypeEnv mm
-}
                putStrLn (renderIndent $ ppr mm)

