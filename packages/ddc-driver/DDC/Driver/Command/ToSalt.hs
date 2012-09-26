
module DDC.Driver.Command.ToSalt
        (cmdToSalt)
where
import DDC.Driver.Bundle
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Core.Module
import DDC.Data.Canned
import System.FilePath
import qualified DDC.Base.Pretty        as P
import qualified Data.Map               as Map


-- | Parse, check, and fully evaluate an expression.
---
--   The Core -> Salt conversion only accepts A-normalised programs,
--   so we normalize it along the way.
--
cmdToSalt :: Config -> Bundle -> Source -> String -> IO ()
cmdToSalt config bundle source sourceText
 | Bundle fragment _ _ _ _ <- bundle
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


-- | Erase the import list of a module.
eraseImports :: Module a n -> Module a n
eraseImports mm
 = mm   { moduleImportKinds     = Map.empty
        , moduleImportTypes     = Map.empty }