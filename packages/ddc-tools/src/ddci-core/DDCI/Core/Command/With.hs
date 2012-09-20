
module DDCI.Core.Command.With
        (cmdWith, cmdWithLite, cmdWithSalt)
where
import DDCI.Core.State
import DDC.Driver.Source
import DDC.Core.Pretty
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Module
import DDC.Data.Canned
import System.Directory
import Control.Monad
import Data.IORef
import Data.Char
import qualified Data.Map       as Map

-- | Add a module to the inliner table.
cmdWith :: State -> Source -> String -> IO State
cmdWith state _source str
 | Bundle frag modules zero simpl rules <- stateBundle state
 = do   res <- cmdWith_load frag str
	case res of
	  Nothing  -> return state
	  Just mdl -> 
		let modules' = Map.insert (moduleName mdl) mdl modules in
		return $ state
		       { stateBundle = Bundle frag modules' zero simpl rules }

cmdWithLite :: State -> Source -> String -> IO State
cmdWithLite state _source str
 = do   res <- cmdWith_load fragmentLite str
	case res of
	  Nothing  -> return state
	  Just mdl ->
		return $ state
		       { stateWithLite = Map.insert (moduleName mdl) mdl (stateWithLite state) }

cmdWithSalt :: State -> Source -> String -> IO State
cmdWithSalt state _source str
 = do   res <- cmdWith_load fragmentSalt str
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

