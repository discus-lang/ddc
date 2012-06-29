
module DDCI.Core.Command.Make
        (cmdMake)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Build.Language
import System.FilePath
import System.Directory
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad
import Data.Maybe
import DDC.Core.Simplifier.Recipie      as Simpl
import qualified DDC.Core.Pretty        as P

data Frag = FragDCL | FragDCE


cmdMake :: State -> Source -> String -> IO ()
cmdMake state source str
 = let  filePath = dropWhile isSpace str
   in   makeFile state source filePath

makeFile state source filePath
        | isSuffixOf ".dcl" filePath
        = makeFrag FragDCL state source filePath

        | isSuffixOf ".dce" filePath
        = makeFrag FragDCE state source filePath

        | otherwise
        = error $ "Don't know how to make " ++ filePath

makeFrag :: Frag -> State -> Source -> FilePath -> IO ()
makeFrag frag state source filePath
 = do   
        -- Read in the source file.
        exists  <- doesFileExist filePath
        when (not exists)
         $ error $ "No such file " ++ show filePath

        src     <- readFile filePath

        -- Decide where to place the build products.
        let outputDir      = fromMaybe (takeDirectory filePath) (stateOutputDir state)
        let outputDirBase  = dropExtension (replaceDirectory filePath outputDir)
        let llPath         = outputDirBase ++ ".ddc.ll"
        let sPath          = outputDirBase ++ ".ddc.s"
        let oPath          = outputDirBase ++ ".o"
        let exePathDefault = outputDirBase
        let exePath        = fromMaybe exePathDefault (stateOutputFile state)

        -- Determine the default builder,
        -- assuming the host and target platforms are the same.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe    (error "Can not determine host platform")
                                        mBuilder

        -- Make the pipeline for the final compilation.
        let pipeLLVM    = PipeLlvmCompile
                        { pipeBuilder           = builder
                        , pipeFileLlvm          = llPath
                        , pipeFileAsm           = sPath
                        , pipeFileObject        = oPath
                        , pipeFileExe           = Just exePath }

        -- Run the pipelines to make the source into a binary.
        errs
         <- case frag of
             FragDCL 
              -> pipeText (nameOfSource source) (lineStartOfSource source) src
               $ PipeTextLoadCore  fragmentLite
               $ pipesCoreLite state builder pipeLLVM

             FragDCE
              -> pipeText (nameOfSource source) (lineStartOfSource source) src
               $ PipeTextLoadCore  fragmentSalt 
               $ pipesCoreSalt state False builder pipeLLVM

        -- Print any errors that arose during compilation.
        mapM_ (putStrLn . P.renderIndent . P.ppr) errs


pipesCoreLite state builder pipeLLVM
 =  [ PipeCoreAsLite 
    [ PipeLiteToSalt    (buildSpec builder)
    [ PipeCoreSimplify  fragmentSalt Simpl.anormalize
    [ PipeCoreCheck     fragmentSalt 
    $ pipesCoreSalt state True builder pipeLLVM]]]]

pipesCoreSalt state doTransfer builder pipeLLVM
 =  [ PipeCoreSimplify  fragmentSalt
                        (stateSimplifier state <> Simpl.anormalize)
    [ PipeCoreCheck     fragmentSalt
    [ PipeCoreAsSalt
    [ (if doTransfer then PipeSaltTransfer else PipeSaltId)
    [ PipeSaltToLlvm   (buildSpec builder) [pipeLLVM]]]]]]

        
