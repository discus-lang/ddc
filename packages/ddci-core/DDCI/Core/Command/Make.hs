
module DDCI.Core.Command.Make
        (cmdMake)
where
import DDCI.Core.Build.Builder
import DDCI.Core.Pipeline.Module
import DDCI.Core.Language
import DDCI.Core.Mode
import DDCI.Core.State
import System.FilePath
import Data.Char
import Data.List
import Data.Monoid
import Data.Maybe
import DDC.Core.Simplifier.Recipie      as Simpl
import qualified DDC.Core.Pretty        as P


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

        src     <- readFile filePath                                    -- TODO check that it exists.

        -- Determine the default builder,
        -- assuming the host and target platforms are the same.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe    (error "Can not determine host platform")
                                        mBuilder

        errs    <- pipeText source src
                $  PipeTextLoadCore  fragmentSea
                [  PipeCoreSimplify  fragmentSea 
                                     (stateSimplifier state <> Simpl.anormalize)
                [  PipeCoreCheck     fragmentSea
                [  PipeCoreAsSea
                [  PipeSeaToLlvm 
                [  PipeLlvmCompile 
                        { pipeBuilder           = builder
                        , pipeFileLlvm          = llPath
                        , pipeFileAsm           = sPath
                        , pipeFileObject        = oPath
                        , pipeFileExe           = Just exePath } ]]]]]

        mapM_ (putStrLn . P.renderIndent . P.ppr) errs
