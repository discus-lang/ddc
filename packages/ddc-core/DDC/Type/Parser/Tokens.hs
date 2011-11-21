
module DDC.Type.Parser.Tokens
        ( Tokens(..)
        , liftTokens
        , tokenStrings
        , readTyConBuiltin
        , readTyConUser
        , readVar)
where
import DDC.Type.Exp
import DDC.Type.Operators.Rename
import DDC.Type.Parser.Lexer
import Control.Monad
import qualified DDC.Type.Compounds     as T


-- | Acceptors for the tokens used by the type parer.
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
        , tTyConBuiltin :: k -> Maybe (TyCon n)
        , tTyConUser    :: k -> Maybe (TyCon n)
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
        , tTyConBuiltin = \k -> liftM (rename (g k)) $ tTyConBuiltin tt $ f k
        , tTyConUser    = \k -> liftM (rename (g k)) $ tTyConUser    tt $ f k
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
        , tTyConBuiltin = readTyConBuiltin
        , tTyConUser    = readTyConUser 
        , tVar          = readVar }


-- | Read a builtin `TyCon`. 
readTyConBuiltin :: String -> Maybe (TyCon n)
readTyConBuiltin ss
 = liftM TyConBuiltin
 $ case ss of
        "Read"          -> Just TyConRead
        "DeepRead"      -> Just TyConDeepRead
        "Write"         -> Just TyConWrite
        "DeepWrite"     -> Just TyConDeepWrite
        "Alloc"         -> Just TyConAlloc
        "Free"          -> Just TyConFree
        "DeepFree"      -> Just TyConDeepFree
        "Const"         -> Just TyConConst
        "DeepConst"     -> Just TyConDeepConst
        "Mutable"       -> Just TyConMutable
        "DeepMutable"   -> Just TyConDeepMutable
        "Lazy"          -> Just TyConLazy
        "HeadLazy"      -> Just TyConHeadLazy
        "Direct"        -> Just TyConDirect
        "Pure"          -> Just TyConPure
        "Empty"         -> Just TyConEmpty
        _               -> Nothing


-- | Read a named, user defined TyCon.
--   We won't know its kind, so fill this in with the Bottom element for 
--   computatation kinds (**0).
readTyConUser :: String -> Maybe (TyCon String)
readTyConUser ss
        | isConName ss  = Just (TyConUser ss (TBot T.sComp))
        | otherwise     = Nothing


-- | Read a named, user defined variable.
readVar :: String -> Maybe String
readVar ss
        | isVarName ss  = Just ss
        | otherwise     = Nothing
