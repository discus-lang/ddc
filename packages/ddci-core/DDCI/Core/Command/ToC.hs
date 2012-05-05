
module DDCI.Core.Command.ToC
        (cmdToC)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Core.Check
import System.FilePath
import DDC.Core.Simplifier.Recipie              as Simpl
import DDC.Core.Simplifier                      as Simpl
import qualified DDC.Core.Salt.Output           as A
import qualified Data.Set                       as Set
import qualified DDC.Base.Pretty                as P
import Data.Maybe
import Data.Monoid

-- | Parse, check, and fully evaluate an expression.
---
--   The Core -> C conversion only accepts A-normalised programs,
--   so we normalize it along the way.
cmdToC :: State -> Source -> String -> IO ()
cmdToC state source str
 | Language fragment    <- stateLanguage state
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        -- Determine the default builder,
        -- assuming the host and target platforms are the same.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe    (error "Can not determine host platform.")
                                        mBuilder

        if      fragName == "Salt" || mSuffix  == Just ".dce"
         then cmdSaltToC state source str
        else if fragName == "Lite"  || mSuffix == Just ".dcl"
         then cmdLiteToC  state source builder str
        else error $ "Don't know how to convert Disciple " ++ fragName ++ " module to C code."


-- | Convert a Disciple Lite module to C code.
cmdLiteToC :: State -> Source -> Builder -> String -> IO ()
cmdLiteToC state source builder str
 = (pipeText (nameOfSource source) (lineStartOfSource source) str
        $  PipeTextLoadCore     fragmentLite
        [  PipeCoreAsLite
        [  PipeLiteToSalt       (buildSpec builder)

        -- The Lite -> Salt conversion adds debruijn indices, 
        -- but these aren't part of the Salt Fragment. 
        --   Run the namifier to eliminate the debruijn indices.
        [  PipeCoreSimplify     fragmentSalt (Simpl.Trans Simpl.Namify)

        [  PipeCoreCheck        fragmentSalt 
        [  pipeCore_saltToC state ]]]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


-- | Convert a Disciple Salt module to C code.
cmdSaltToC :: State -> Source -> String -> IO ()
cmdSaltToC state source str
 = (pipeText (nameOfSource source) (lineStartOfSource source) str
        $  PipeTextLoadCore     fragmentSalt
        [  pipeCore_saltToC state])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)


pipeCore_saltToC :: Show a => State -> PipeCore (AnTEC a A.Name) A.Name
pipeCore_saltToC state
        =  PipeCoreSimplify    fragmentSalt
                               (stateSimplifier state <> Simpl.anormalize)
        [  PipeCoreReCheck     fragmentSalt
        [  PipeCoreAsSalt
        [  PipeSaltPrint 
                (Set.member SaltPrelude (stateModes state))
                SinkStdout ]]]
