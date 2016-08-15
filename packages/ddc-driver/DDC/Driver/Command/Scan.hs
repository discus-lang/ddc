module DDC.Driver.Command.Scan
        ( cmdScanFromFile)
where
import DDC.Driver.Stage
import DDC.Source.Tetra.Lexer           as ST
import DDC.Data.Token                   as Token
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.Directory
import Control.Monad


-------------------------------------------------------------------------------
-- | Scan a module.
--   Any errors are thrown in the `ExceptT` monad.
cmdScanFromFile 
        :: Config               -- ^ Driver config.
        -> FilePath             -- ^ Module file name.
        -> Bool                 -- ^ Whether to print source locations.
        -> ExceptT String IO ()

cmdScanFromFile _config filePath bPrintLocs
 = do
        -- Check that the file exists.
        exists  <- liftIO $ doesFileExist filePath
        when (not exists)
         $ throwE $ "No such file " ++ show filePath

        -- Read in the source file.
        src     <- liftIO $ readFile filePath

        -- Lex the source string.
        let toks    = ST.lexModuleString filePath 1 src

        if bPrintLocs 
         then liftIO $ putStr $ unlines $ map show toks
         else liftIO $ putStr $ unlines $ map show $ map Token.tokenTok toks

