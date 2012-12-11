
module DDCI.Core.Command.With
        (cmdWith, cmdWithLite, cmdWithSalt)
where
import DDCI.Core.State
import DDC.Driver.Source
import DDC.Core.Pretty
import DDC.Build.Pipeline
import DDC.Core.Module
import DDC.Data.Canned
import System.Directory
import Control.Monad
import Data.IORef
import Data.Char
import qualified Data.Map                       as Map
import qualified DDC.Build.Language.Lite        as Lite
import qualified DDC.Build.Language.Salt        as Salt


-- | Add a module to the inliner table.
cmdWith :: State -> Source -> String -> IO State
cmdWith state _source str
 | Language bundle      <- stateLanguage  state
 , modules              <- bundleModules  bundle
 , fragment             <- bundleFragment bundle
 = do   res <- cmdWith_load fragment str
	case res of
	  Nothing  -> return state
	  Just mdl 
           -> do
		let modules' = Map.insert (moduleName mdl) mdl modules 
                let bundle'  = bundle { bundleModules = modules' }
		return $ state { stateLanguage = Language bundle' }


cmdWithLite :: State -> Source -> String -> IO State
cmdWithLite state _source str
 = do   res <- cmdWith_load Lite.fragment str
	case res of
	  Nothing  -> return state
	  Just mdl ->
		return $ state
		       { stateWithLite = Map.insert (moduleName mdl) mdl (stateWithLite state) }

cmdWithSalt :: State -> Source -> String -> IO State
cmdWithSalt state _source str
 = do   res <- cmdWith_load Salt.fragment str
	case res of
	  Nothing  -> return state
	  Just mdl ->
		return $ state
		       { stateWithSalt = Map.insert (moduleName mdl) mdl (stateWithSalt state) }


cmdWith_load frag str
 = do	-- Always treat the string as a filename
        let source   = SourceFile str

        -- Read in the source file.
        let filePath = dropWhile isSpace str
        exists  <- doesFileExist filePath
        when (not exists)
         $      error $ "No such file " ++ show filePath

        src     <- readFile filePath

	cmdWith_parse frag source src


cmdWith_parse frag source src
 = do   ref     <- newIORef Nothing
        errs    <- pipeText (nameOfSource source) (lineStartOfSource source) src
                $  PipeTextLoadCore frag
                   [ PipeCoreHacks (Canned (\m -> writeIORef ref (Just m) >> return m)) 
                     [PipeCoreOutput SinkDiscard] ]

        case errs of
         [] -> do
                putStrLn "ok"
                readIORef ref

         _ -> do
                mapM_ (putStrLn . renderIndent . ppr) errs
                return Nothing

