
module DDC.Core.Codec.Text.Lexer.Token.Keyword
        ( Keyword (..)
        , sayKeyword
        , scanKeyword)
where
import Text.Lexer.Inchworm.Char
import DDC.Core.Codec.Text.Lexer.Token.Names


-------------------------------------------------------------------------------
-- | Keyword tokens.
data Keyword
        -- core keywords.
        = EModule
        | EImport
        | EExport
        | EForeign
        | EType
        | ECapability
        | EValue
        | EData
        | EWith
        | EWhere
        | EIn
        | ELet
        | ELetCase
        | ELetRec
        | ERec
        | EPrivate
        | EExtend
        | EUsing
        | ECase
        | EOf
        | EWeakEff
        | EWeakClo
        | EPurify
        | EForget
        | EBox
        | ERun
        | EAsync

        -- sugar keywords.
        | EDo
        | EMatch
        | EIf
        | EThen
        | EElse
        | EOtherwise
        | EConstant
        | EMutable
        deriving (Eq, Show)


-------------------------------------------------------------------------------
-- | Yield the string name of a keyword.
sayKeyword :: Keyword -> String
sayKeyword kw
 = case kw of
        -- core keywords.
        EBox            -> "box"
        ECapability     -> "capability"
        ECase           -> "case"
        EData           -> "data"
        EExport         -> "export"
        EExtend         -> "extend"
        EForeign        -> "foreign"
        EForget         -> "forget"
        EImport         -> "import"
        EIn             -> "in"
        ELet            -> "let"
        ELetCase        -> "letcase"
        ELetRec         -> "letrec"
        ERec            -> "rec"
        EModule         -> "module"
        EOf             -> "of"
        EPrivate        -> "private"
        EPurify         -> "purify"
        ERun            -> "run"
        EType           -> "type"
        EValue          -> "value"
        EWhere          -> "where"
        EWeakClo        -> "weakclo"
        EWeakEff        -> "weakeff"
        EWith           -> "with"
        EUsing          -> "using"
        EAsync          -> "async"

        -- sugar keywords
        EDo             -> "do"
        EElse           -> "else"
        EIf             -> "if"
        EMatch          -> "match"
        EOtherwise      -> "otherwise"
        EThen           -> "then"
        EConstant       -> "constant"
        EMutable        -> "mutable"


-------------------------------------------------------------------------------
-- | Scanner for a `Keyword`.
scanKeyword :: Scanner IO Location [Char] (Location, Keyword)
scanKeyword
 = munchPred Nothing matchVarName acceptKeyword

-- | Accept a keyword token.
acceptKeyword :: String -> Maybe Keyword
acceptKeyword str
 = case str of
        -- core keywords
        "box"           -> Just EBox
        "capability"    -> Just ECapability
        "case"          -> Just ECase
        "data"          -> Just EData
        "export"        -> Just EExport
        "extend"        -> Just EExtend
        "foreign"       -> Just EForeign
        "forget"        -> Just EForget
        "import"        -> Just EImport
        "in"            -> Just EIn
        "let"           -> Just ELet
        "letcase"       -> Just ELetCase
        "letrec"        -> Just ELetRec
        "rec"           -> Just ERec
        "module"        -> Just EModule
        "of"            -> Just EOf
        "private"       -> Just EPrivate
        "purify"        -> Just EPurify
        "run"           -> Just ERun
        "type"          -> Just EType
        "value"         -> Just EValue
        "where"         -> Just EWhere
        "weakclo"       -> Just EWeakClo
        "weakeff"       -> Just EWeakEff
        "with"          -> Just EWith
        "using"         -> Just EUsing
        "async"         -> Just EAsync

        -- sugar keywords
        "do"            -> Just EDo
        "else"          -> Just EElse
        "if"            -> Just EIf
        "match"         -> Just EMatch
        "otherwise"     -> Just EOtherwise
        "then"          -> Just EThen
        "constant"      -> Just EConstant
        "mutable"       -> Just EMutable

        _               -> Nothing

