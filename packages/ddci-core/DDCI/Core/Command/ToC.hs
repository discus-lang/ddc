
module DDCI.Core.Command.ToC
        (cmdToC)
where
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDCI.Core.Language
import DDC.Core.Fragment.Profile
import System.FilePath
import DDC.Core.Simplifier.Recipie              as Simpl
import qualified DDC.Core.Brine.Output          as E
import qualified Data.Set                       as Set
import qualified DDC.Base.Pretty                as P
import Data.Monoid


-- | Parse, check, and fully evaluate an expression.
---
--   The Core -> C conversion only accepts A-normalised programs,
--   so we normalize it along the way.
cmdToC :: State -> Source -> String -> IO ()
cmdToC state source str
 | Language fragment    <- stateLanguage state
 = let  fragName = profileName (fragmentProfile fragment)
        mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

   in   if      fragName == "Brine" || mSuffix == Just ".dce"
         then cmdBrineToC state source str
        else if fragName == "Lite"  || mSuffix == Just ".dcl"
         then cmdLiteToC  state source str
        else error $ "Don't know how to convert Disciple " ++ fragName ++ " module to C code."


-- | Convert a Disciple Lite module to C code.
cmdLiteToC :: State -> Source -> String -> IO ()
cmdLiteToC state source str
 = (pipeText source str
        $  PipeTextLoadCore     fragmentLite
        [  PipeCoreAsLite
        [  PipeLiteToBrine
        [  pipeCore_brineToC state ]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


-- | Convert a Disciple Brine module to C code.
cmdBrineToC :: State -> Source -> String -> IO ()
cmdBrineToC state source str
 = (pipeText source str
        $  PipeTextLoadCore     fragmentBrine
        [  pipeCore_brineToC state])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


pipeCore_brineToC :: State -> PipeCore E.Name
pipeCore_brineToC state
        = PipeCoreSimplify     fragmentBrine   
                               (stateSimplifier state <> Simpl.anormalize)
        [  PipeCoreCheck       fragmentBrine
        [  PipeCoreAsBrine
        [  PipeBrinePrint 
                (Set.member BrinePrelude (stateModes state))
                SinkStdout ]]]
