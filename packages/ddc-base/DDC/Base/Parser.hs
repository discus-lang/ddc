
module DDC.Base.Parser
        ( module Text.Parsec
        , module Text.Parsec.Pos
        , ParserG
        , ParserStateG(..)
        , runWrapParserG
        , pToken
        , pTokenAs
        , pTok)
where
import Text.Parsec
import Text.Parsec.Pos
import Data.Functor.Identity


-- | A generic parser,
--   parameterised over token definition type, token type, name type and return type.
type ParserG ts k n a
        =  (Show k, Eq k)
        => ParsecT [k] (ParserStateG ts k n) Identity a


-- | A generic parser state.
data ParserStateG ts k n
        = ParseState
        { stateTokens           :: ts
        , stateTokenShow        :: k -> String
        , stateTokenPos         :: k -> SourcePos
        , stateMakeName         :: k -> String -> n
        , stateFileName         :: String }


-- | Run a generic parser,
--   where the tokens are just wrapped strings.
runWrapParserG
        :: (Eq k, Show k, Ord n)
        => ts                   -- ^ Token definitions
        -> (k -> String)        -- ^ Show a token.
        -> (k -> SourcePos)     -- ^ Take the source position of a token.
        -> (k -> String -> n)   -- ^ Convert a string to a variable name.
        -> String               -- ^ File name for error messages.
        -> ParserG ts k n a     -- ^ Parser to run.
        -> [k]                  -- ^ Tokens to parse.
        -> Either ParseError a

runWrapParserG ts tokenShow tokenPos makeName fileName parser 
 = runParser parser
        ParseState 
        { stateTokens           = ts
        , stateTokenShow        = tokenShow
        , stateTokenPos         = tokenPos
        , stateMakeName         = makeName
        , stateFileName         = fileName }
        fileName


-- | Accept a token from the table.
pToken  :: (ts -> k -> Maybe a)
        -> ParserG ts k n a
pToken  f
 = do   state   <- getState
        token   (stateTokenShow state)
                (stateTokenPos  state)
                (f (stateTokens state))


-- | Accept a token from the table and return the given value.
pTokenAs  :: (ts -> k -> Bool) -> t
          -> ParserG ts k n t
pTokenAs f t = pTok f >> return t


-- | Accept a token from the table.
pTok    :: (ts -> k -> Bool)
        -> ParserG ts k n ()
pTok f  = pToken $ \toks t -> if f toks t then Just () else Nothing

