
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


-- | A description of the tokens the parser accepts.
--   This is abstract in the exact representation, so that the client module can 
--   attach its own source positions and file name information.
data Tokens k
        = Tokens
        { tRoundBra     :: k
        , tRoundKet     :: k
        , tSquareBra    :: k
        , tSquareKet    :: k
        , tColon        :: k
        , tComma        :: k
        , tDot          :: k
        , tPlus         :: k
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
        , tTyConBuiltin :: k -> Maybe (TyCon k)
        , tTyConUser    :: k -> Maybe (TyCon k)
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
        , tTyConBuiltin = liftM (rename toTok) . tTyConBuiltin tt . fromTok 
        , tTyConUser    = liftM (rename toTok) . tTyConUser    tt . fromTok 
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
        , tTyConBuiltin = readTyConBuiltin
        , tTyConUser    = readTyConUser 
        , tVar          = readVar }


-- | Read a builtin `TyCon`. 
readTyConBuiltin :: String -> Maybe (TyCon k)
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

