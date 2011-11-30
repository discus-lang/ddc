
module DDC.Base.Lexer
        ( SourcePos     (..)
        
          -- * Tokens
        , Token         (..)
        , takeParsecSourcePos)
where
import DDC.Base.Pretty
import qualified Text.Parsec.Pos        as P


-- SourcePos ------------------------------------------------------------------
-- | A position in the source file.        
--
--   If there is no file path then we assume that the input has been read
--   from an interactive session and display '<interactive>' when pretty printing.
data SourcePos 
        = SourcePos
        { sourcePosFile         :: Maybe FilePath
        , sourcePosLine         :: Int
        , sourcePosColumn       :: Int }
        deriving (Eq, Show)


instance Pretty SourcePos where
 ppr (SourcePos (Just f) l c)	
	= ppr $ f ++ ":"         ++ show l ++ ":" ++ show c

 ppr (SourcePos Nothing  l c)	
	= ppr $ "<interactive>:" ++ show l ++ ":" ++ show c


-- Token-----------------------------------------------------------------------
-- | Wrapper for a token type that gives it a source position.
data Token t
        = Token
        { tokenTok         :: t
        , tokenSourcePos  :: SourcePos }
        deriving (Eq, Show)


-- | Take the parsec style 
takeParsecSourcePos :: Token k -> P.SourcePos
takeParsecSourcePos (Token _ sp)
 = case sp of
        SourcePos Nothing  l c   -> P.newPos "<interactive>" l c
        SourcePos (Just f) l c   -> P.newPos f l c
