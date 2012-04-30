
module DDCI.Core.Command.Compile
        (cmdCompile)
where
import DDCI.Core.Build.Builder
import DDCI.Core.Pipeline.Module
import DDCI.Core.Language
import DDCI.Core.Mode
import DDCI.Core.State
import System.FilePath
import System.Directory
import Control.Monad
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
 = do   -- Read in the source file.
        exists  <- doesFileExist filePath
        when (not exists)
         $ error $ "No such file " ++ show filePath

        src     <- readFile filePath

        -- Decide where to put the build products.
        let outputDir      = fromMaybe (takeDirectory filePath) (stateOutputDir state)
        let outputDirBase  = dropExtension (replaceDirectory filePath outputDir)
        let llPath         = outputDirBase ++ ".ddc.ll"
        let sPath          = outputDirBase ++ ".ddc.s"
        let oPathDefault   = outputDirBase ++ ".o"
        let oPath          = fromMaybe oPathDefault (stateOutputFile state)

        -- Determine the default builder,
        -- assuming the host and target platforms are the same.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe    (error "Can not determine host platform.")
                                        mBuilder

        -- Run the build pipeline.
        errs    <- pipeText source src
                $  PipeTextLoadCore  fragmentSalt
                [  PipeCoreSimplify  fragmentSalt
                                     (stateSimplifier state <> Simpl.anormalize)
                [  PipeCoreCheck     fragmentSalt
                [  PipeCoreAsSalt
                [  PipeSaltToLlvm   (buildSpec builder)
                [  PipeLlvmCompile   
                        { pipeBuilder           = builder
                        , pipeFileLlvm          = llPath
                        , pipeFileAsm           = sPath
                        , pipeFileObject        = oPath
                        , pipeFileExe           = Nothing } ]]]]]

        mapM_ (putStrLn . P.renderIndent . P.ppr) errs

