
module DDC.Data.Token
        ( Token(..)
        , takeParsecSourcePos
        , tokenLine
        , tokenColumn)
where
import DDC.Data.SourcePos
import qualified Text.Parsec.Pos        as P


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

