
module DDC.Data.SourcePos
        (SourcePos (..))
where
import DDC.Base.Pretty


-- | A position in the source file.        
--
--   If there is no file path then we assume that the input has been read
--   from an interactive session and display ''<interactive>'' when pretty printing.
data SourcePos 
        = SourcePos
        { sourcePosSource       :: String
        , sourcePosLine         :: Int
        , sourcePosColumn       :: Int }
        deriving (Eq, Show)


instance Pretty SourcePos where
 ppr (SourcePos source l c)     
        = ppr $ source ++ ":" ++ show l ++ ":" ++ show c

