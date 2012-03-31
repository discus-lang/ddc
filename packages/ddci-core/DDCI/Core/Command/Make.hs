
module DDCI.Core.Command.Make
        (cmdMake)
where
import DDCI.Core.Build.Builder
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDC.Core.Pretty
import Data.List
import System.FilePath
import Data.Char


cmdMake :: State -> Int -> String -> IO ()
cmdMake state lineStart str
 = let  filePath = dropWhile isSpace str
   in   makeFile state lineStart filePath

makeFile state lineStart filePath
        | isSuffixOf ".dce" filePath
        = makeDCE state lineStart filePath

        | otherwise
        = error $ "Don't know how to make " ++ filePath

 
makeDCE :: State -> Int -> FilePath -> IO ()
makeDCE _state lineStart filePath
 = do   let llPath      = replaceExtension filePath ".ddc.ll"
        let sPath       = replaceExtension filePath ".ddc.s"
        let oPath       = replaceExtension filePath ".o"
        let exePath     = "Main"

        -- TODO check that it exists.
        src     <- readFile filePath

        -- TODO: refactor to take Source instead of lineStart
        errs    <- pipeTextModule lineStart src
                $  PipeTextModuleLoadSea
                [  PipeSeaModuleToLlvm 
                [  PipeLlvmModuleCompile 
                        { pipeBuilder           = builder_I386_Darwin
                        , pipeFileLlvm          = llPath
                        , pipeFileAsm           = sPath
                        , pipeFileObject        = oPath
                        , pipeFileExe           = exePath } ]]

        mapM_ (putStrLn . renderIndent . ppr) errs
