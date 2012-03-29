
module DDCI.Core.Build.Make
        ( makeFile
        , Builder(..))
where
import DDCI.Core.Build.Builder
import DDC.Core.Pretty
import DDC.Core.Sea.Output.Profile
import DDCI.Core.State
import Data.List
import System.FilePath
import qualified DDC.Core.Sea.Output.Name       as E
import qualified DDC.Core.Llvm.Platform         as L
import qualified DDC.Core.Llvm.Convert          as L
import qualified DDCI.Core.Pipeline.Fragment    as I
import qualified DDC.Core.Load                  as C


-- | Parse, check and convert a Sea module to LLVM.
makeFile :: State -> FilePath -> IO ()
makeFile state filePath
 | isSuffixOf ".dce" filePath
 = do   _       <- makeDCE state filePath
        return ()

 | otherwise
 = error $ "don't know what to do with" ++ filePath


-- Error ----------------------------------------------------------------------
data Error      
        -- | Error when parsing and typechecking module.
        = ErrorLoad     (C.Error E.Name)

        -- | Error in fragment check.
        | ErrorFragment String
        deriving Show


-- DCE ------------------------------------------------------------------------
-- | Make a DCE file.
makeDCE :: State -> FilePath -> IO (Maybe Error)
makeDCE _state filePath
 = do   file     <- readFile filePath
        let toks = lexString 0 file
        goLoad toks

 where  goLoad toks
         = case C.loadModule outputProfile filePath  toks of
                Left err -> return $ Just $ ErrorLoad err
                Right mm -> goFragmentCheck mm

        goFragmentCheck mm
         = case I.fragmentCheckModule mm of
                Just err -> return $ Just $ ErrorFragment err
                Nothing  -> goLLVM mm

        goLLVM mm
         = case L.convertModule L.platform32 mm of
                mm'      -> goAssemble (renderIndent $ ppr mm')

        goAssemble llSrc
         = do   -- Write out the LLVM source file.
                let llPath      = replaceExtension filePath ".ddc.ll"
                writeFile llPath llSrc

                -- Compile LLVM source file into .s file.
                let builder     = builder_I386_Darwin
                let sPath       = replaceExtension filePath ".ddc.s"
                buildLlc builder llPath sPath

                -- Assemble .s file into .o file
                let oPath       = replaceExtension filePath ".o"
                buildAs  builder sPath oPath

                -- Link .o file into a executable
                let exePath     = "Main"
                buildLdExe builder oPath exePath

                return Nothing

