
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
import DDC.Base.Pretty
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec              as P
import Text.Parsec.Error        as P


-- | A generic parser,
--   parameterised over token definition type, token type, name type and return type.
type Parser k a
        =  Eq k
        => P.ParsecT [Token k] (ParserState k) Identity a


-- | A generic parser state.
data ParserState k
        = ParseState
        { stateTokenShow        :: k -> String
        , stateFileName         :: String }


-- | Run a generic parser,
--   where the tokens are just wrapped strings.
runTokenParser
        :: Eq k
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


-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
instance Pretty P.ParseError where
 ppr err
  = vcat $  [  text "Parse error in" <+> text (show (P.errorPos err)) ]
         ++ (map ppr $ packMessages $ P.errorMessages err)
         
         
instance Pretty P.Message where
 ppr msg
  = case msg of
        SysUnExpect str -> text "Unexpected" <+> text str <> text "."
        UnExpect    str -> text "Unexpected" <+> text str <> text "."
        Expect      str -> text "Expected"   <+> text str <> text "."
        Message     str -> text str


-- | When we get a parse error, parsec adds multiple 'Unexpected' messages,
--   but we only want to display the first one.
packMessages :: [P.Message] -> [P.Message]
packMessages mm
 = case mm of
        []      -> []
        m1@(P.UnExpect _)   :  (P.UnExpect _)    : rest
                -> packMessages (m1 : rest)

        m1@(P.SysUnExpect _) : (P.SysUnExpect _) : rest
                -> packMessages (m1 : rest)

        m1@(P.SysUnExpect _) : (P.UnExpect _)    : rest
                -> packMessages (m1 : rest)

        m1@(P.UnExpect _)    : (P.SysUnExpect _) : rest
                -> packMessages (m1 : rest)

        m1 : rest
                -> m1 : packMessages rest

