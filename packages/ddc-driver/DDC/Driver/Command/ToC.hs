
module DDC.Driver.Command.ToC
        (cmdToC)
where
import DDC.Driver.Bundle
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment
import System.FilePath
import qualified DDC.Base.Pretty        as P


-- | Parse, check, and convert a module to C.
--
--   The output is printed to @stdout@. 
--
cmdToC  :: Config       -- ^ Compiler configuration.
        -> Bundle       -- ^ Language bundle.
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> IO ()

cmdToC config bundle source sourceText
 | Bundle fragment _ _ _  _ <- bundle
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        -- Decide what to do based on file extension and current fragment.
        let compile
                -- Compile a Core Lite module.
                | fragName == "Lite" || mSuffix == Just ".dcl"
                = pipeText (nameOfSource source) (lineStartOfSource source) sourceText
                $ PipeTextLoadCore fragmentLite
                [ stageLiteToSalt  config source 
                [ stageSaltToC     config source SinkStdout]]

                -- Compile a Core Salt module.
                | fragName == "Salt" || mSuffix == Just ".dce"
                = pipeText (nameOfSource source) (lineStartOfSource source) sourceText
                $ PipeTextLoadCore fragmentSalt
                [ stageSaltToC     config source SinkStdout]

                -- Unrecognised.
                | otherwise
                = error $ "Don't know how to convert this to C"


        -- Print any errors that arose during compilation
        errs <- compile

        mapM_ (putStrLn . P.renderIndent . P.ppr) errs
