
module DDCI.Core.Command.Compile
        (cmdCompile)
where
import DDCI.Core.Build.Builder
import DDCI.Core.Pipeline.Module
import DDCI.Core.Language
import DDCI.Core.Mode
import DDCI.Core.State
import System.FilePath
import Data.List
import Data.Char
import Data.Monoid
import Data.Maybe
import DDC.Core.Simplifier.Recipie      as Simpl
import qualified DDC.Core.Pretty        as P


cmdCompile :: State -> Source -> String -> IO ()
cmdCompile state source str
 = let  filePath = dropWhile isSpace str
   in   compileFile state source filePath

compileFile state source filePath
        | isSuffixOf ".dce" filePath
        = compileDCE state source filePath

        | otherwise
        = error $ "Don't know how to compile " ++ filePath

 
compileDCE :: State -> Source -> FilePath -> IO ()
compileDCE state source filePath
 = do   let llPath      = replaceExtension filePath ".ddc.ll"
        let sPath       = replaceExtension filePath ".ddc.s"
        let oPath       = replaceExtension filePath ".o"

        src             <- readFile filePath            -- TODO check that it exists.

        -- Determine the default builder,
        -- assuming the host and target platforms are the same.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe    (error "Can not determine host platform.")
                                        mBuilder

        errs    <- pipeText source src
                $  PipeTextLoadCore  fragmentSea
                [  PipeCoreSimplify  fragmentSea
                                     (stateSimplifier state <> Simpl.anormalize)
                [  PipeCoreCheck     fragmentSea
                [  PipeCoreAsSea
                [  PipeSeaToLlvm     (buildSpec builder)
                [  PipeLlvmCompile   
                        { pipeBuilder           = builder
                        , pipeFileLlvm          = llPath
                        , pipeFileAsm           = sPath
                        , pipeFileObject        = oPath
                        , pipeFileExe           = Nothing } ]]]]]

        mapM_ (putStrLn . P.renderIndent . P.ppr) errs
