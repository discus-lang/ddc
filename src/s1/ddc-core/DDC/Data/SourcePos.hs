{-# LANGUAGE TypeFamilies #-}
module DDC.Data.SourcePos
        ( SourcePos             (..)
        , Located               (..)
        , sourcePosOfLocated
        , parsecSourcePosOfLocated
        , nameOfLocated
        , lineOfLocated
        , columnOfLocated
        , valueOfLocated)
where
import DDC.Data.Pretty
import Control.DeepSeq
import qualified Text.Parsec.Pos        as Parsec

-- | A position in a source file.
--
--   If there is no file path then we assume that the input has been read
--   from an interactive session and display ''<interactive>'' when pretty printing.
data SourcePos
        = SourcePos
        { sourcePosSource       :: !String
        , sourcePosLine         :: !Int
        , sourcePosColumn       :: !Int }
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


-- | A located thing.
data Located a
        = Located !SourcePos !a
        deriving (Eq, Show)


-- | Take the source position of a located thing.
sourcePosOfLocated :: Located a -> SourcePos
sourcePosOfLocated (Located sp _)       = sp


-- | Take the parsec source position of a located thing.
parsecSourcePosOfLocated :: Located a -> Parsec.SourcePos
parsecSourcePosOfLocated
        (Located (SourcePos name l col) _)
        = Parsec.newPos name l col


-- | Yield the source name of a located thing.
nameOfLocated :: Located a -> String
nameOfLocated (Located (SourcePos name _ _) _) = name


-- | Yield the line number of a located thing.
lineOfLocated :: Located a -> Int
lineOfLocated (Located (SourcePos _ l _) _)    = l


-- | Yield the column number of a located thing.
columnOfLocated :: Located a -> Int
columnOfLocated (Located (SourcePos _ _ c) _) = c


-- | Yield the contained value of a located thing.
valueOfLocated  :: Located a -> a
valueOfLocated (Located _ x)    = x
