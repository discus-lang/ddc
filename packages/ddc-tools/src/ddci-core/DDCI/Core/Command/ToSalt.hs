
module DDCI.Core.Command.ToSalt
        (cmdToSalt)
where
import DDCI.Core.Interface.Suppress
import DDCI.Core.State
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Data.Canned
import System.FilePath
import qualified DDC.Base.Pretty                as P


-- | Parse, check, and fully evaluate an expression.
---
--   The Core -> Salt conversion only accepts A-normalised programs,
--   so we normalize it along the way.
cmdToSalt :: State -> Source -> String -> IO ()
cmdToSalt state source str
 | Bundle fragment _ _ _ _ <- stateBundle state
 = do   let fragName = profileName (fragmentProfile fragment)
        let mSuffix  = case source of 
                        SourceFile filePath     -> Just $ takeExtension filePath
                        _                       -> Nothing

        -- Determine the builder to use.
        builder         <- getActiveBuilder state

        -- Slurp out the driver config we need from the DDCI state.
        let config      = driverConfigOfState state

        -- Decide what to do based on file extension and current fragment.
        let compile
                -- Compile a Core Lite module.
                | fragName == "Lite" || mSuffix == Just ".dcl"
                = pipeText (nameOfSource source) (lineStartOfSource source) str
                $ PipeTextLoadCore fragmentLite
                [ stageLiteToSalt  config source builder
                [ (if configSuppressCoreImports config
                        then PipeCoreHacks    (Canned (\x -> return $ eraseImports x))
                        else PipeCoreId)
                [ PipeCoreOutput   SinkStdout]]]

                -- Unrecognised.
                | otherwise
                = error $ "Don't know how to convert this to Salt"

        -- Print any errors that arose during compilation
        errs <- compile

        mapM_ (putStrLn . P.renderIndent . P.ppr) errs
