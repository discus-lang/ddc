
module DDC.Type.Parser.Tokens
        ( Tokens(..)
        , tokenStrings
        , readTyConBuiltin
        , readTyConUser
        , readVar)
where
import DDC.Type.Exp
import DDC.Type.Parser.Lexer
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
 = case ss of
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
        | isTyConName ss        = Just (TyConData ss (TBot T.sComp))
        | otherwise             = Nothing


-- | Read a named, user defined variable.
readVar :: String -> Maybe String
readVar ss
        | isTyVarName ss        = Just ss
        | otherwise             = Nothing

