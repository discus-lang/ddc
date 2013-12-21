
module DDCI.Tetra.Command.ToCore
        (cmdToCore)
where
import DDCI.Tetra.State
import DDCI.Tetra.Mode
import DDC.Interface.Source
import DDC.Base.Pretty
import DDC.Source.Tetra.Env
import DDC.Source.Tetra.Lexer
import DDC.Source.Tetra.Parser
import DDC.Source.Tetra.Pretty          ()
import DDC.Source.Tetra.Desugar.Defix
import DDC.Source.Tetra.Infer.Expand            as Expand
import DDC.Source.Tetra.ToCore                  as ToCore
import qualified DDC.Core.Transform.SpreadX     as C
import qualified DDC.Core.Tetra.Env             as C
import qualified DDC.Core.Tetra                 as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Lexer                 as C
import qualified DDC.Base.Parser                as BP
import qualified DDC.Data.SourcePos             as SP
import qualified Data.Set                       as Set
import Control.Monad


cmdToCore :: State -> Source -> String -> IO ()
cmdToCore state source str
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
         = do   let sp          = SP.SourcePos "<top level>" 1 1
                
                -- Convert Source Tetra into Core Tetra.
                let mm_core     = ToCore.toCoreModule sp mm

                -- Spread types of data constructors into uses.
                let mm_spread   = C.spreadX C.primKindEnv C.primTypeEnv mm_core
                
                goSynth mm_spread


        goSynth mm
         = let  config                  = C.configOfProfile C.profile
                (mResult, _docTrace)    = C.checkModule config mm C.Synth
           in do
                when (Set.member Dump $ stateModes state)
                 $ writeFile "dump.tetra-core.dct" (renderIndent $ ppr mm)
                
                case mResult of
                 Left err       
                  -> putStrLn (renderIndent $ ppr err)
                 
                 Right mm'      
                  -> putStrLn (renderIndent $ ppr mm')


