
module DDCI.Tetra.Command.ToCore
        (cmdToCore)
where
import DDCI.Tetra.State
import DDC.Interface.Source
import DDC.Base.Pretty
import DDC.Source.Tetra.Env
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty          ()
import DDC.Source.Tetra.Desugar.Defix
import DDC.Source.Tetra.Infer.Expand            as Expand
import DDC.Source.Tetra.ToCore                  as ToCore
import qualified DDC.Build.Pipeline             as B
import qualified DDC.Driver.Dump                as D
import qualified DDC.Driver.Config              as D
import qualified DDC.Core.Transform.SpreadX     as C
import qualified DDC.Core.Tetra.Env             as C
import qualified DDC.Core.Tetra                 as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Base.Parser                as BP
import qualified DDC.Data.SourcePos             as SP


-- Convert Disciple Source Tetra text into Disciple Core Tetra.
cmdToCore :: State -> D.Config -> Source -> String -> IO ()
cmdToCore _state configDriver source str
 = goLex

 where  -- Lex the input text into source tokens.
        goLex
         = let  tokens  = lexModuleString (nameOfSource source) 1 str
           in   goParse tokens

        -- Parse the source tokens.
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


        -- Desugar the source program, 
        -- rewriting infix operators to prefix.
        goDesugar mm
         = case defix defaultFixTable mm of
            Left err    -> putStrLn (renderIndent $ ppr err)
            Right mm'   -> goExpand mm'


        -- Expand missing quantifiers in type signatures.
        goExpand mm
         = do   let mm' = Expand.expand Expand.configDefault 
                                primKindEnv primTypeEnv mm
                goToCore mm'


        -- Convert Tetra Source to Tetra Core.
        goToCore mm
         = do   -- TODO: get proper source location when we're 
                -- converting programs from a ddci-tetra session.
                let sp          = SP.SourcePos "<top level>" 1 1
                
                -- Convert Source Tetra into Core Tetra.
                let mm_core     = ToCore.toCoreModule sp mm

                -- Spread types of data constructors into uses.
                let mm_spread   = C.spreadX C.primKindEnv C.primTypeEnv mm_core
                
                goSynth mm_spread


        -- Synthesise missing type annotations in the core module.
        goSynth mm
         = let  configCheck             = C.configOfProfile C.profile
                (mResult, checkTrace)   = C.checkModule configCheck mm C.Synth
           in do
                -- Dump type checker trace.
                B.pipeSink (renderIndent $ C.checkTraceDoc checkTrace)
                           (D.dump configDriver source "dump.check-trace.txt")

                -- Dump checked core file.
                B.pipeSink (renderIndent $ ppr mm)
                           (D.dump configDriver source "dump.tetra-core.dct")
                
                case mResult of
                 Left err       
                  -> putStrLn (renderIndent $ ppr err)
                 
                 Right mm'      
                  -> putStrLn (renderIndent $ ppr mm')


