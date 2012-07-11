
module DDCI.Core.Command.With
        (cmdWith)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Core.Pretty
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Module
import DDC.Core.Fragment.Profile
import DDC.Data.Canned
import System.Directory
import System.FilePath
import Control.Monad
import Data.IORef
import Data.Char
import qualified Data.Map       as Map

-- | Add a module to the inliner table.
cmdWith :: State -> Source -> String -> IO State
cmdWith state _source str
 = do   
        -- Always treat the string as a filename
        let source   = SourceFile str

        -- Read in the source file.
        let filePath = dropWhile isSpace str
        exists  <- doesFileExist filePath
        when (not exists)
         $      error $ "No such file " ++ show filePath

        src     <- readFile filePath

        cmdWith_src state source filePath src


cmdWith_src state source filePath src
        | '.' : ext                <- takeExtension filePath
        , Just (Language fragment) <- languageOfExtension ext
        = case profileName $ fragmentProfile fragment of
                "Lite"  -> cmdWith_lite state source src
                "Salt"  -> cmdWith_salt state source src
                _       -> error $ "cannot load " ++ show filePath

        | otherwise
        = error $ "cannot load" ++ show filePath


cmdWith_lite state source src
 = do   ref     <- newIORef (error "cmdWith_src failed")

        errs    <- pipeText (nameOfSource source) (lineStartOfSource source) src
                $  PipeTextLoadCore fragmentLite
                   [ PipeCoreHacks (Canned (\m -> writeIORef ref m >> return m)) 
                     [PipeCoreOutput SinkDiscard] ]

        case errs of
         [] -> do
                putStrLn "ok"
                mdl     <- readIORef ref
                return $ state
                       { stateWithLite = Map.insert (moduleName mdl) mdl (stateWithLite state) }

         _ -> do
                mapM_ (putStrLn . renderIndent . ppr) errs
                return state


cmdWith_salt state source src
 = do   ref     <- newIORef (error "cmdWith_src failed")

        errs    <- pipeText (nameOfSource source) (lineStartOfSource source) src
                $  PipeTextLoadCore fragmentSalt
                   [ PipeCoreHacks (Canned (\m -> writeIORef ref m >> return m)) 
                     [PipeCoreOutput SinkDiscard] ]

        case errs of
         [] -> do
                putStrLn "ok"
                mdl     <- readIORef ref
                return $ state
                       { stateWithSalt = Map.insert (moduleName mdl) mdl (stateWithSalt state) }

         _ -> do
                mapM_ (putStrLn . renderIndent . ppr) errs
                return state








