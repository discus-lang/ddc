
module DDC.Data.SourcePos
        (SourcePos (..))
where
import DDC.Base.Pretty
import Control.DeepSeq

-- | A position in a source file.        
--
--   If there is no file path then we assume that the input has been read
--   from an interactive session and display ''<interactive>'' when pretty printing.
data SourcePos 
        = SourcePos
        { sourcePosSource       :: String
        , sourcePosLine         :: Int
        , sourcePosColumn       :: Int }
        deriving (Eq, Show)


instance NFData SourcePos where
 rnf (SourcePos str l c)
  = rnf str `seq` rnf l `seq` rnf c


instance Pretty SourcePos where
 -- Suppress printing of line and column number when they are both zero.
 -- File line numbers officially start from 1, so having 0 0 probably
 -- means this isn't real information.
 ppr (SourcePos source 0 0)
        = text $ source

 ppr (SourcePos source l c)     
        = text $ source ++ ":" ++ show l ++ ":" ++ show c

