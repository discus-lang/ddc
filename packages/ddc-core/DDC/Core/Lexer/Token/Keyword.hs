
module DDC.Core.Lexer.Token.Keyword
        ( Keyword (..)
        , sayKeyword)
where

-- | Keywords.
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

        -- sugar keywords.
        | EDo
        | EMatch
        | EIf
        | EThen
        | EElse
        | EOtherwise
        deriving (Eq, Show)


sayKeyword :: Keyword -> String
sayKeyword kw
 = case kw of
        -- core keywords.
        EModule         -> "module"
        EImport         -> "import"
        EExport         -> "export"
        EForeign        -> "foreign"
        EType           -> "type"
        ECapability     -> "capability"
        EValue          -> "value"
        EData           -> "data"
        EWith           -> "with"
        EWhere          -> "where"
        EIn             -> "in"
        ELet            -> "let"
        ELetCase        -> "letcase"
        ELetRec         -> "letrec"
        EPrivate        -> "private"
        EExtend         -> "extend"
        EUsing          -> "using"
        ECase           -> "case"
        EOf             -> "of"
        EWeakEff        -> "weakeff"
        EWeakClo        -> "weakclo"
        EPurify         -> "purify"
        EForget         -> "forget"
        EBox            -> "box"
        ERun            -> "run"

        -- sugar keywords
        EDo             -> "do"
        EMatch          -> "match"
        EIf             -> "if"
        EThen           -> "then"
        EElse           -> "else"
        EOtherwise      -> "otherwise"

