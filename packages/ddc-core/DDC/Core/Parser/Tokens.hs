
module DDC.Core.Parser.Tokens
        ( Tokens(..)
        , liftTokens
        , tokenStrings)
where
import DDC.Core.Exp
import DDC.Type.Exp
import DDC.Type.Operators.Rename
import Control.Monad
import qualified DDC.Type.Parser.Tokens as T
import qualified DDC.Type.Parser.Lexer  as T

-- | A description of the tokens the parser accepts.
--
--   * This is a conservative extension of the type tokens.
--   * This is abstract in the exact representation, so that the client module can 
--     attach its own source positions and file name information.
data Tokens k
        = Tokens
        { tRoundBra     :: k
        , tRoundKet     :: k
        , tSquareBra    :: k
        , tSquareKet    :: k
        , tColon        :: k
        , tDot          :: k
        , tComma        :: k
        , tPlus         :: k
        , tBackSlash    :: k
        , tSemiColon    :: k
        , tSortComp     :: k
        , tSortProp     :: k
        , tKindValue    :: k
        , tKindRegion   :: k
        , tKindEffect   :: k
        , tKindClosure  :: k
        , tKindWitness  :: k
        , tKindFun      :: k
        , tTypeFun      :: k
        , tTypeFunBra   :: k
        , tTypeFunKet   :: k
        , tBotEffect    :: k
        , tBotClosure   :: k
        , tLetRec       :: k
        , tLet          :: k
        , tLocal        :: k
        , tIn           :: k
        , tPurify       :: k
        , tEmptify      :: k
        , tCase         :: k
        , tOf           :: k
        , tTyConBuiltin :: k -> Maybe (TyCon k)
        , tTyConUser    :: k -> Maybe (TyCon k)
        , tWiConBuiltin :: k -> Maybe WiCon 
        , tDaCon        :: k -> Maybe k
        , tVar          :: k -> Maybe k }


-- | Apply a function to all the tokens in the table.
liftTokens :: Ord k2 => (k1 -> k2) -> (k2 -> k1) -> Tokens k1 -> Tokens k2
liftTokens toTok fromTok tt
        = Tokens
        { tRoundBra     = toTok (tRoundBra    tt)
        , tRoundKet     = toTok (tRoundKet    tt)
        , tSquareBra    = toTok (tSquareBra   tt)
        , tSquareKet    = toTok (tSquareKet   tt)
        , tColon        = toTok (tColon       tt)
        , tComma        = toTok (tComma       tt)
        , tDot          = toTok (tDot         tt)
        , tPlus         = toTok (tPlus        tt)
        , tBackSlash    = toTok (tBackSlash   tt)
        , tSemiColon    = toTok (tSemiColon   tt)
        , tSortComp     = toTok (tSortComp    tt)
        , tSortProp     = toTok (tSortProp    tt)
        , tKindValue    = toTok (tKindValue   tt)
        , tKindRegion   = toTok (tKindRegion  tt)
        , tKindEffect   = toTok (tKindEffect  tt)
        , tKindClosure  = toTok (tKindClosure tt)
        , tKindWitness  = toTok (tKindWitness tt)
        , tKindFun      = toTok (tKindFun     tt)
        , tTypeFun      = toTok (tTypeFun     tt)
        , tTypeFunBra   = toTok (tTypeFunBra  tt)
        , tTypeFunKet   = toTok (tTypeFunKet  tt)
        , tBotEffect    = toTok (tBotEffect   tt)
        , tBotClosure   = toTok (tBotClosure  tt)
        , tLetRec       = toTok (tLetRec      tt)
        , tLet          = toTok (tLet         tt)
        , tLocal        = toTok (tLocal       tt)
        , tIn           = toTok (tIn          tt)
        , tPurify       = toTok (tPurify      tt)
        , tEmptify      = toTok (tEmptify     tt)
        , tCase         = toTok (tCase        tt)
        , tOf           = toTok (tOf          tt)
        , tTyConBuiltin = liftM (rename toTok) . tTyConBuiltin tt . fromTok 
        , tTyConUser    = liftM (rename toTok) . tTyConUser    tt . fromTok 
        , tWiConBuiltin = tWiConBuiltin tt . fromTok
        , tDaCon        = liftM toTok . tVar tt . fromTok
        , tVar          = liftM toTok . tVar tt . fromTok }


-- | An instance of the `Tokens` type, that just uses strings for the tokens.
tokenStrings :: Tokens String
tokenStrings
        = Tokens
        { tRoundBra     = "("
        , tRoundKet     = ")"
        , tSquareBra    = "["
        , tSquareKet    = "]"
        , tColon        = ":"
        , tComma        = ","
        , tDot          = "."
        , tPlus         = "+"
        , tBackSlash    = "\\"
        , tSemiColon    = ";"
        , tSortComp     = "**"
        , tSortProp     = "@@"
        , tKindValue    = "*"
        , tKindRegion   = "%"
        , tKindEffect   = "!"
        , tKindClosure  = "$"
        , tKindWitness  = "@"
        , tKindFun      = "~>"
        , tTypeFun      = "->"
        , tTypeFunBra   = "-("
        , tTypeFunKet   = ")>"
        , tBotEffect    = "!0"
        , tBotClosure   = "$0"
        , tLetRec       = "letrec"
        , tLet          = "let"
        , tLocal        = "local"
        , tIn           = "in"
        , tPurify       = "purify"
        , tEmptify      = "emptify"
        , tCase         = "case"
        , tOf           = "of"
        , tTyConBuiltin = T.readTyConBuiltin
        , tTyConUser    = T.readTyConUser 
        , tWiConBuiltin = readWiConBuiltin
        , tDaCon        = readDaConUser
        , tVar          = T.readVar }


-- | Read a `WiCon`.
readWiConBuiltin :: String -> Maybe WiCon
readWiConBuiltin ss
 = case ss of
        "MkPure"        -> Just WiConMkPure
        "MkEmpty"       -> Just WiConMkEmpty
        "MkConst"       -> Just WiConMkConst
        "MkMutable"     -> Just WiConMkMutable
        "MkLazy"        -> Just WiConMkLazy
        "MkDirect"      -> Just WiConMkDirect
        "MkPurify"      -> Just WiConMkPurify
        "MkShare"       -> Just WiConMkShare
        _               -> Nothing
        -- TODO: mkDistinct


-- | Read a named, user defined TyCon.
--   We won't know its kind, so fill this in with the Bottom element for 
--   computatation kinds (**0).
readDaConUser :: String -> Maybe String
readDaConUser ss
        | T.isConName ss  = Just ss
        | otherwise       = Nothing
