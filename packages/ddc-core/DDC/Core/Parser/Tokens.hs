
module DDC.Core.Parser.Tokens
        ( Tokens(..)
        , liftTokens
        , tokenStrings
        , readWiConBuiltin)
where
import DDC.Core.Exp
import DDC.Type.Operators.Rename
import Control.Monad
import qualified DDC.Type.Parser.Tokens as T
import qualified DDC.Type.Parser.Lexer  as T


-- | Acceptors for the tokens used by the core language parser.
--
--   * This is a conservative extension of the type tokens.
--   * This is abstract in the exact representation, so that the client module can 
--     attach its own source positions and file name information.
data Tokens k n
        = Tokens
        { tRoundBra     :: k -> Bool
        , tRoundKet     :: k -> Bool
        , tSquareBra    :: k -> Bool
        , tSquareKet    :: k -> Bool
        , tColon        :: k -> Bool
        , tDot          :: k -> Bool
        , tComma        :: k -> Bool
        , tPlus         :: k -> Bool
        , tBackSlash    :: k -> Bool
        , tSemiColon    :: k -> Bool
        , tSortComp     :: k -> Bool
        , tSortProp     :: k -> Bool
        , tKindValue    :: k -> Bool
        , tKindRegion   :: k -> Bool
        , tKindEffect   :: k -> Bool
        , tKindClosure  :: k -> Bool
        , tKindWitness  :: k -> Bool
        , tKindFun      :: k -> Bool
        , tTypeFun      :: k -> Bool
        , tTypeFunBra   :: k -> Bool
        , tTypeFunKet   :: k -> Bool
        , tBotEffect    :: k -> Bool
        , tBotClosure   :: k -> Bool
        , tLetRec       :: k -> Bool
        , tLet          :: k -> Bool
        , tLocal        :: k -> Bool
        , tIn           :: k -> Bool
        , tPurify       :: k -> Bool
        , tEmptify      :: k -> Bool
        , tCase         :: k -> Bool
        , tOf           :: k -> Bool
        , tTyConBuiltin :: k -> Maybe (TyCon n)
        , tTyConUser    :: k -> Maybe (TyCon n)
        , tWiConBuiltin :: k -> Maybe WiCon
        , tDaCon        :: k -> Maybe n
        , tVar          :: k -> Maybe n }


-- | Apply a function to all the tokens in the table.
liftTokens 
        :: Ord n2 
        => (k2 -> k1)
        -> (k2 -> n1 -> n2)
        -> Tokens k1 n1
        -> Tokens k2 n2

liftTokens f g tt
        = Tokens
        { tRoundBra     = tRoundBra    tt . f
        , tRoundKet     = tRoundKet    tt . f
        , tSquareBra    = tSquareBra   tt . f
        , tSquareKet    = tSquareKet   tt . f
        , tColon        = tColon       tt . f
        , tComma        = tComma       tt . f
        , tDot          = tDot         tt . f
        , tPlus         = tPlus        tt . f
        , tBackSlash    = tBackSlash   tt . f
        , tSemiColon    = tSemiColon   tt . f
        , tSortComp     = tSortComp    tt . f
        , tSortProp     = tSortProp    tt . f
        , tKindValue    = tKindValue   tt . f
        , tKindRegion   = tKindRegion  tt . f
        , tKindEffect   = tKindEffect  tt . f
        , tKindClosure  = tKindClosure tt . f
        , tKindWitness  = tKindWitness tt . f
        , tKindFun      = tKindFun     tt . f
        , tTypeFun      = tTypeFun     tt . f
        , tTypeFunBra   = tTypeFunBra  tt . f
        , tTypeFunKet   = tTypeFunKet  tt . f
        , tBotEffect    = tBotEffect   tt . f
        , tBotClosure   = tBotClosure  tt . f
        , tLetRec       = tLetRec      tt . f
        , tLet          = tLet         tt . f
        , tLocal        = tLocal       tt . f
        , tIn           = tIn          tt . f
        , tPurify       = tPurify      tt . f
        , tEmptify      = tEmptify     tt . f
        , tCase         = tCase        tt . f
        , tOf           = tOf          tt . f
        , tTyConBuiltin = \k -> liftM (rename (g k)) $ tTyConBuiltin tt $ f k
        , tTyConUser    = \k -> liftM (rename (g k)) $ tTyConUser    tt $ f k 
        , tWiConBuiltin = \k ->                        tWiConBuiltin tt $ f k 
        , tDaCon        = \k -> liftM (g k)          $ tDaCon tt        $ f k
        , tVar          = \k -> liftM (g k)          $ tVar   tt        $ f k }


-- | An instance of the `Tokens` type, that just uses strings for the tokens.
tokenStrings :: Tokens String String
tokenStrings
        = Tokens
        { tRoundBra     = (==) "("
        , tRoundKet     = (==) ")"
        , tSquareBra    = (==) "["
        , tSquareKet    = (==) "]"
        , tColon        = (==) ":"
        , tComma        = (==) ","
        , tDot          = (==) "."
        , tPlus         = (==) "+"
        , tBackSlash    = (==) "\\"
        , tSemiColon    = (==) ";"
        , tSortComp     = (==) "**"
        , tSortProp     = (==) "@@"
        , tKindValue    = (==) "*"
        , tKindRegion   = (==) "%"
        , tKindEffect   = (==) "!"
        , tKindClosure  = (==) "$"
        , tKindWitness  = (==) "@"
        , tKindFun      = (==) "~>"
        , tTypeFun      = (==) "->"
        , tTypeFunBra   = (==) "-("
        , tTypeFunKet   = (==) ")>"
        , tBotEffect    = (==) "!0"
        , tBotClosure   = (==) "$0"
        , tLetRec       = (==) "letrec"
        , tLet          = (==) "let"
        , tLocal        = (==) "local"
        , tIn           = (==) "in"
        , tPurify       = (==) "purify"
        , tEmptify      = (==) "emptify"
        , tCase         = (==) "case"
        , tOf           = (==) "of"
        , tTyConBuiltin = T.readTyConBuiltin
        , tTyConUser    = T.readTyConUser 
        , tWiConBuiltin = readWiConBuiltin
        , tDaCon        = readDaConUser
        , tVar          = T.readVar }



-- | Read a named, user defined TyCon.
--   We won't know its kind, so fill this in with the Bottom element for 
--   computatation kinds (**0).
readDaConUser :: String -> Maybe String
readDaConUser ss
        | T.isConName ss  = Just ss
        | otherwise       = Nothing


-- | Read a `WiCon`.
readWiConBuiltin :: String -> Maybe WiCon
readWiConBuiltin ss
 = case ss of
        "pure"          -> Just WiConPure
        "empty"         -> Just WiConEmpty
        "const"         -> Just WiConConst
        "mutable"       -> Just WiConMutable
        "lazy"          -> Just WiConLazy
        "direct"        -> Just WiConDirect
        "read"          -> Just WiConRead
        "free"          -> Just WiConFree
        _               -> Nothing
