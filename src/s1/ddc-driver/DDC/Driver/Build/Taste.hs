
module DDC.Driver.Build.Taste where

import Control.Monad
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Codec.Text.Lexer      as C
import qualified DDC.Source.Discus.Module       as SD
import qualified DDC.Source.Discus.Lexer        as SD
import qualified DDC.Source.Discus.Parser       as SD
import qualified DDC.Control.Parser             as BP
import qualified System.FilePath                as S


-- | Taste the header of a module.
tasteNeeded
        :: FilePath     -- ^ Path of module.
        -> String       -- ^ Module source.
        -> IO (Maybe (C.ModuleName, [C.ModuleName]))

tasteNeeded filePath src
 | S.takeExtension filePath == ".ds"
 = do
        -- Lex the module, dropping all tokens after and including
        -- the first 'where', because we only need the module header.
        let tokens
                = dropBody
                $ SD.lexModuleString filePath 1 src

        case BP.runTokenParser
                C.describeToken
                filePath SD.pModule tokens of
         Left  err  -> error $ "parse error " ++ show err
         Right mm
          -> do
                -- Check that the module name matches the file path where
                -- we found the module. If they don't match then the compilation
                -- driver will go into a loop as it can never load a module
                -- with the name it needs.
                when (not $ C.moduleNameMatchesPath filePath (SD.moduleName mm))
                 $ error $ unlines
                 [ "Module name does not match file path."
                 , "  module name = " ++ show (SD.moduleName mm)
                 , "  file path   = " ++ show filePath ]

                return  $ Just
                        ( SD.moduleName mm
                        , SD.moduleImportModules mm)
 | otherwise
 = return Nothing


-- | Drop tokens after and including the first 'where' keyword.
--   When parsing just the module header we can drop these tokens
--   because they only represent the body of the module.
dropBody :: [C.Located (C.Token n)] -> [C.Located (C.Token n)]
dropBody toks = go toks
 where  go []           = []
        go (C.Located _ (C.KA (C.KKeyword C.EWhere)) : _) = []
        go (t : moar)   = t : go moar
