{-# LANGUAGE TypeFamilies #-}

-- | Parser utilities.
module DDC.Control.Parser
        ( module Text.Parsec
        , Parser
        , ParserState   (..)
        , D.SourcePos   (..)
        , runTokenParser
        , pTokMaybe,    pTokMaybeSP
        , pTokAs,       pTokAsSP
        , pTok,         pTokSP)
where
import DDC.Data.Pretty
import DDC.Data.SourcePos       as D
import Data.Functor.Identity
import Text.Parsec              hiding (SourcePos)
import Text.Parsec              as P  
import Text.Parsec.Error        as P


-- | A generic parser,
--   parameterised over token and return types.
type Parser k a
        =  Eq k
        => P.ParsecT [Located k] (ParserState k) Identity a


-- | A parser state that keeps track of the name of the source file.
data ParserState k
        = ParseState
        { stateTokenShow        :: k -> String
        , stateFileName         :: String }


-- | Run a generic parser, making sure all input is consumed.
runTokenParser
        :: Eq k
        => (k -> String)        -- ^ Show a token.
        -> String               -- ^ File name for error messages.
        -> Parser k a           -- ^ Parser to run.
        -> [Located k]          -- ^ Tokens to parse.
        -> Either P.ParseError a

runTokenParser tokenShow fileName parser 
 = P.runParser eofParser
        ParseState 
        { stateTokenShow        = tokenShow
        , stateFileName         = fileName }
        fileName
 where
  eofParser
   = do r <- parser
        -- We can't use primitive Text.Parsec.eof because it requires
        -- @Show (Token k)@
        (do
                c <- pTokMaybe Just
                unexpected (tokenShow c)) <|> return r


-------------------------------------------------------------------------------
-- | Accept the given token.
pTok   :: k -> Parser k ()
pTok k  = pTokMaybe $ \k' -> if k == k' then Just () else Nothing


-- | Accept the given token, returning its source position.
pTokSP :: k -> Parser k D.SourcePos
pTokSP k  
 = do   (_, sp) <- pTokMaybeSP 
                $ \k' -> if k == k' then Just () else Nothing
        return sp


-- | Accept a token and return the given value.
pTokAs :: k -> t -> Parser k t
pTokAs k t 
 = do   pTok k
        return t


-- | Accept a token and return the given value, 
--   along with the source position of the token.
pTokAsSP :: k -> t -> Parser k (t, D.SourcePos)
pTokAsSP k t 
 = do   sp      <- pTokSP k
        return  (t, sp)


-- | Accept a token if the function returns `Just`. 
pTokMaybe :: (k -> Maybe a) -> Parser k a
pTokMaybe f 
 = do   state   <- P.getState

        P.token (stateTokenShow state . valueOfLocated)
                (parsecSourcePosOfLocated)
                (f . valueOfLocated)


-- | Accept a token if the function return `Just`, 
--   also returning the source position of that token.
pTokMaybeSP  :: (k -> Maybe a) -> Parser k (a, D.SourcePos)
pTokMaybeSP f
 = do   state   <- P.getState

        let f' token' 
                = case f (valueOfLocated token') of
                        Nothing -> Nothing
                        Just x  -> Just (x, sourcePosOfLocated token')

        P.token (stateTokenShow state . valueOfLocated)
                (parsecSourcePosOfLocated)
                f'


-------------------------------------------------------------------------------
instance Pretty P.ParseError where
 data PrettyMode P.ParseError   = PrettyParseError
 pprDefaultMode                 = PrettyParseError
 ppr err
  = vcat $  [  text "Parse error in" <+> text (show (P.errorPos err)) ]
         ++ (map ppr $ packMessages $ P.errorMessages err)
         
         
instance Pretty P.Message where
 data PrettyMode P.Message      = PrettyMessage
 pprDefaultMode                 = PrettyMessage
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

