
module DDCI.Core.Command.ToC
        (cmdToC)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDCI.Core.Stage
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import System.FilePath
import qualified DDC.Base.Pretty                as P

-- | Parse, check, and convert a module to C.
--
cmdToC :: State -> Source -> String -> IO ()
cmdToC state source str
 | Bundle fragment _ _ _  _ <- stateBundle state
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        -- Determine the builder to use.
        builder         <- getActiveBuilder state

        -- Decide what to do based on file extension and current fragment.
        let compile
                -- Compile a Core Lite module.
                | fragName == "Lite" || mSuffix == Just ".dcl"
                = pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore fragmentLite
                [ stageLiteToSalt  state source builder
                [ stageSaltToC     state source builder SinkStdout]]

                -- Compile a Core Salt module.
                | fragName == "Salt" || mSuffix == Just ".dce"
                = pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore fragmentSalt
                [ stageSaltToC     state source builder SinkStdout]

                -- Unrecognised.
                | otherwise
                = error $ "Don't know how to convert this to C"


        -- Print any errors that arose during compilation
        errs <- compile

        mapM_ (putStrLn . P.renderIndent . P.ppr) errs
