
module DDC.Base.Parser
        ( module Text.Parsec
        , Parser
        , ParserState   (..)
        , runTokenParser
        , pTokMaybe
        , pTokAs
        , pTok)
where
import DDC.Base.Lexer
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec                      as P


-- | A generic parser,
--   parameterised over token definition type, token type, name type and return type.
type Parser k a
        =  (Show k, Eq k)
        => P.ParsecT [Token k] (ParserState k) Identity a


-- | A generic parser state.
data ParserState k
        = ParseState
        { stateTokenShow        :: k -> String
        , stateFileName         :: String }


-- | Run a generic parser,
--   where the tokens are just wrapped strings.
runTokenParser
        :: (Eq k, Show k)
        => (k -> String)        -- ^ Show a token.
        -> String               -- ^ File name for error messages.
        -> Parser k a           -- ^ Parser to run.
        -> [Token k]            -- ^ Tokens to parse.
        -> Either P.ParseError a

runTokenParser tokenShow fileName parser 
 = P.runParser parser
        ParseState 
        { stateTokenShow        = tokenShow
        , stateFileName         = fileName }
        fileName


-- | Accept a token.
pTokMaybe  :: (k -> Maybe a) -> Parser k a
pTokMaybe f
 = do   state   <- P.getState
        P.token (stateTokenShow state . tokenTok)
                (takeParsecSourcePos)
                (f . tokenTok)


-- | Accept a token and return the given value.
pTokAs    :: Eq k => k -> t -> Parser k t
pTokAs k t = pTok k >> return t


-- | Accept the given token.
pTok      :: Eq k => k -> Parser k ()
pTok k  = pTokMaybe $ \k' -> if k == k' then Just () else Nothing

