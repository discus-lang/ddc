
-- | Lexer utilities.
module DDC.Base.Lexer
        ( SourcePos     (..)
        
          -- * Tokens
        , Token         (..)
        , tokenLine
        , tokenColumn
        , takeParsecSourcePos)
where
import DDC.Base.Pretty
import qualified Text.Parsec.Pos        as P


-- SourcePos ------------------------------------------------------------------
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


-- Token-----------------------------------------------------------------------
-- | Wrapper for primitive token type that gives it a source position.
data Token t
        = Token
        { tokenTok         :: t
        , tokenSourcePos  :: SourcePos }
        deriving (Eq, Show)


-- | Take the parsec style source position from a token.
takeParsecSourcePos :: Token k -> P.SourcePos
takeParsecSourcePos (Token _ sp)
 = case sp of
        SourcePos source l c
         -> P.newPos source l c


-- | Take the line number of a token.
tokenLine :: Token t -> Int
tokenLine (Token _ (SourcePos _ l _))   = l


-- | Take the column number of a token.
tokenColumn :: Token t -> Int
tokenColumn (Token _ (SourcePos _ _ c)) = c

