
module DDCI.Core.Command.Make
        (cmdMake)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDCI.Core.Stage
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Build.Language
import System.Directory
import Data.Char
import Data.List
import Control.Monad
import Data.Maybe
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
        -- Determine the builder to use.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe (error "Can not determine host platform")
                                     mBuilder

        -- Read in the source file.
        exists  <- doesFileExist filePath
        when (not exists)
         $ error $ "No such file " ++ show filePath

        src       <- readFile filePath

        -- Run the pipelines to make the source into a binary.
        errs
         <- case frag of
             FragDCL 
              -> pipeText (nameOfSource source) (lineStartOfSource source) src
               $ PipeTextLoadCore  fragmentLite
               [ stageLiteToSalt   state builder
               [ stageSaltToLLVM   state builder True 
               [ stageCompileLLVM  state builder filePath ]]]

             FragDCE
              -> pipeText (nameOfSource source) (lineStartOfSource source) src
               $ PipeTextLoadCore  fragmentSalt 
               [ stageSaltToLLVM   state builder False 
               [ stageCompileLLVM  state builder filePath ]]

        -- Print any errors that arose during compilation.
        mapM_ (putStrLn . P.renderIndent . P.ppr) errs

