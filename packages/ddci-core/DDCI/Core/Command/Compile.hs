
module DDCI.Core.Command.Compile
        (cmdCompile)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDCI.Core.Stage
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Build.Language
import System.Directory
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import qualified DDC.Core.Pretty        as P


cmdCompile :: State -> Source -> String -> IO ()
cmdCompile state source str
 = do   -- Read in the source file.
        let filePath = dropWhile isSpace str
        exists  <- doesFileExist filePath
        when (not exists)
         $      error $ "No such file " ++ show filePath

        src     <- readFile filePath

        -- Determine the builder to use.
        mBuilder        <- determineDefaultBuilder defaultBuilderConfig
        let builder     =  fromMaybe (error "Can not determine host platform")
                                     mBuilder

        -- Decide what to do based on file extension.
        let make
                -- Make a Core Lite module.
                | isSuffixOf ".dcl" filePath
                = pipeText (nameOfSource source) (lineStartOfSource source) src
                $ PipeTextLoadCore  fragmentLite
                [ stageLiteToSalt   state builder
                [ stageSaltToLLVM   state builder True 
                [ stageCompileLLVM  state builder filePath False ]]]

                -- Make a Core Salt module.
                | isSuffixOf ".dce" filePath
                = pipeText (nameOfSource source) (lineStartOfSource source) src
                $ PipeTextLoadCore  fragmentSalt 
                [ stageSaltToLLVM   state builder False 
                [ stageCompileLLVM  state builder filePath False ]]

                -- Unrecognised.
                | otherwise
                = error $ "Don't know how to make " ++ filePath

        -- Print any errors that arose during compilation.
        errs    <- make
        mapM_ (putStrLn . P.renderIndent . P.ppr) errs
