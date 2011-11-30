
module DDC.Type.Parser.Tokens
        ( Tokens(..)
        , liftTokens
        , tokenStrings
        , readTwConBuiltin
        , readTcConBuiltin
        , readTcConData
        , readVar)
where
import DDC.Type.Exp
import DDC.Type.Transform.Rename
import DDC.Type.Parser.Lexer
import Control.Monad
import qualified DDC.Type.Compounds     as T


-- | Acceptors for the tokens used by the type parser.
-- 
--   This is abstract in the exact representation, so that the client module can 
--   attach its own source positions and file name information.
data Tokens k n
        = Tokens
        { tRoundBra     :: k -> Bool
        , tRoundKet     :: k -> Bool
        , tSquareBra    :: k -> Bool
        , tSquareKet    :: k -> Bool
        , tColon        :: k -> Bool
        , tComma        :: k -> Bool
        , tDot          :: k -> Bool
        , tPlus         :: k -> Bool
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
        , tTwConBuiltin :: k -> Maybe TwCon
        , tTcConBuiltin :: k -> Maybe (TcCon n)
        , tTcConData    :: k -> Maybe (TcCon n)
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
        { tRoundBra     = tRoundBra     tt . f
        , tRoundKet     = tRoundKet     tt . f
        , tSquareBra    = tSquareBra    tt . f
        , tSquareKet    = tSquareKet    tt . f
        , tColon        = tColon        tt . f
        , tComma        = tComma        tt . f
        , tDot          = tDot          tt . f
        , tPlus         = tPlus         tt . f
        , tSortComp     = tSortComp     tt . f
        , tSortProp     = tSortProp     tt . f
        , tKindValue    = tKindValue    tt . f
        , tKindRegion   = tKindRegion   tt . f
        , tKindEffect   = tKindEffect   tt . f
        , tKindClosure  = tKindClosure  tt . f
        , tKindWitness  = tKindWitness  tt . f
        , tKindFun      = tKindFun      tt . f
        , tTypeFun      = tTypeFun      tt . f
        , tTypeFunBra   = tTypeFunBra   tt . f
        , tTypeFunKet   = tTypeFunKet   tt . f
        , tBotEffect    = tBotEffect    tt . f
        , tBotClosure   = tBotClosure   tt . f
        , tTwConBuiltin = tTwConBuiltin tt . f
        , tTcConBuiltin = \k -> liftM (rename (g k)) $ tTcConBuiltin tt $ f k
        , tTcConData    = \k -> liftM (rename (g k)) $ tTcConData    tt $ f k
        , tVar          = \k -> liftM (g k)          $ tVar tt          $ f k }


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
        , tTwConBuiltin = readTwConBuiltin
        , tTcConBuiltin = readTcConBuiltin
        , tTcConData    = readTcConData
        , tVar          = readVar }


-- | Read a named, builtin `TwCon`. 
readTwConBuiltin :: String -> Maybe TwCon
readTwConBuiltin ss
 = case ss of
        "Const"         -> Just TwConConst
        "DeepConst"     -> Just TwConDeepConst
        "Mutable"       -> Just TwConMutable
        "DeepMutable"   -> Just TwConDeepMutable
        "Lazy"          -> Just TwConLazy
        "HeadLazy"      -> Just TwConHeadLazy
        "Direct"        -> Just TwConDirect
        "Pure"          -> Just TwConPure
        "Empty"         -> Just TwConEmpty
        _               -> Nothing


-- | Read a named, builtin `TcCon`. 
readTcConBuiltin :: String -> Maybe (TcCon n)
readTcConBuiltin ss
 = case ss of
        "Read"          -> Just TcConRead
        "DeepRead"      -> Just TcConDeepRead
        "Write"         -> Just TcConWrite
        "DeepWrite"     -> Just TcConDeepWrite
        "Alloc"         -> Just TcConAlloc
        "Free"          -> Just TcConFree
        "DeepFree"      -> Just TcConDeepFree
        _               -> Nothing


-- | Read a named, user defined `TcCon`.
--
--   We won't know its kind, so fill this in with the Bottom element for 
--   computatation kinds (**0).
readTcConData :: String -> Maybe (TcCon String)
readTcConData ss
        | isConName ss  = Just (TcConData ss (TBot T.sComp))
        | otherwise     = Nothing


-- | Read a named, user defined variable.
readVar :: String -> Maybe String
readVar ss
        | isVarName ss  = Just ss
        | otherwise     = Nothing
