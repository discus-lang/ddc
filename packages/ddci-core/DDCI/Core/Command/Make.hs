
module DDCI.Core.Command.Make
        (cmdMake)
where
import DDCI.Core.Build.Builder
import DDCI.Core.Pipeline.Module
import DDCI.Core.Language
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Core.Pretty
import Data.List
import System.FilePath
import Data.Char


cmdMake :: State -> Source -> String -> IO ()
cmdMake state source str
 = let  filePath = dropWhile isSpace str
   in   makeFile state source filePath

makeFile state source filePath
        | isSuffixOf ".dce" filePath
        = makeDCE state source filePath

        | otherwise
        = error $ "Don't know how to make " ++ filePath

 
makeDCE :: State -> Source -> FilePath -> IO ()
makeDCE state source filePath
 = do   let llPath      = replaceExtension filePath ".ddc.ll"
        let sPath       = replaceExtension filePath ".ddc.s"
        let oPath       = replaceExtension filePath ".o"
        let exePath     = "Main"

        -- TODO check that it exists.
        src     <- readFile filePath

        errs    <- pipeTextModule source src
                $  PipeTextModuleLoadCore fragmentSea
                [  PipeCoreModuleSimplify  (stateSimplifier state)
                [  PipeCoreModuleAsSea
                [  PipeSeaModuleToLlvm 
                [  PipeLlvmModuleCompile 
                        { pipeBuilder           = builder_I386_Darwin
                        , pipeFileLlvm          = llPath
                        , pipeFileAsm           = sPath
                        , pipeFileObject        = oPath
                        , pipeFileExe           = exePath } ]]]]

        mapM_ (putStrLn . renderIndent . ppr) errs
