{-# LANGUAGE OverloadedStrings #-}

module DDC.Core.Salt.Convert.Name
        ( seaNameOfSuper
        , seaNameOfLocal
        , sanitizeName)
where
import DDC.Core.Salt.Name
import DDC.Core.Module
import DDC.Type.Exp.Simple.Exp
import DDC.Data.Pretty
import Data.Maybe
import qualified DDC.Core.Module.Import as C
import qualified DDC.Core.Module.Export as C
import qualified Data.Text              as T
import qualified Data.List              as L


-- | Convert the Salt name of a supercombinator to a name we can use when
--   defining the C function.
seaNameOfSuper
        :: Maybe (ImportValue Name (Type Name)) -- ^ How the super is imported
        -> Maybe (ExportValue Name (Type Name)) -- ^ How the super is exported
        -> Name                                 -- ^ Name of the super.
        -> Maybe Doc

seaNameOfSuper mImport mExport nm

        -- Super is defined in this module and not exported.
        | Nothing               <- mImport
        , Nothing               <- mExport
        , Just str              <- takeNameVar nm
        = Just $ text $ sanitizeName $ T.unpack str

        -- Special case for the main function.
        -- This comes with a export definition but we don't want
        -- the 'Main' module name in the symbol.
        -- Super is exported to other modules.
        | Just ev@ExportValueLocal{} <- mExport
        , ModuleName ps         <- C.exportValueLocalModuleName ev
        , Just str              <- takeNameVar nm
        , ps  == ["Main"]
        , str == "main"
        = Just $ text $ T.unpack str

        -- Super is exported to other modules.
        | Just ev@ExportValueLocal{} <- mExport
        , ModuleName ps         <- C.exportValueLocalModuleName ev
        , Just str              <- takeNameVar nm
        = Just $ text $ sanitizeName (L.intercalate "." ps ++ "." ++ T.unpack str)

        -- Super is imported from another module.
        | Just iv@ImportValueModule{} <- mImport
        , ModuleName ps         <- C.importValueModuleName iv
        , Just str              <- takeNameVar nm
        = Just $ text $ sanitizeName (L.intercalate "." ps ++ "." ++ T.unpack str)

        -- Super is imported from C-land and not exported.
        | Just (ImportValueSea _ strSea _) <- mImport
        , Nothing               <- mExport
        = Just $ text $ T.unpack strSea

        -- ISSUE #320: Handle all the import/export combinations.
        --
        -- We don't handle the other cases because we would need to
        -- produce a wrapper to convert the names.
        | Just str                      <- takeNameVar nm
        = Just $ text $ T.unpack str

        | otherwise
        = Nothing


-- | Convert the Salt name of a local variable to a name we can use in the
--   body of a C function.
seaNameOfLocal :: Name -> Maybe Doc
seaNameOfLocal nn
 = case takeNameVar nn of
        Just str -> Just $ text $ "_" ++ sanitizeName (T.unpack str)
        _        -> Nothing


-- Sanitize ---------------------------------------------------------------------------------------
-- | Rewrite a name to make it safe to export as an external C symbol.
--
--   Names containing unfriendly characters like '&' are prefixed with '_sym_'
--   and the '&' is replaced by 'ZAn'. Literal 'Z's such a name are doubled to 'ZZ'.
--
sanitizeName :: String -> String
sanitizeName str
 = concatMap rewriteChar str


-- | Get the encoded version of a character.
rewriteChar :: Char -> String
rewriteChar c
 = fromMaybe [c] $ convertSymbol c


-- | Convert symbols to their sanitized form.
convertSymbol :: Char -> Maybe String
convertSymbol c
 = case c of
        'Z'     -> Just "ZZ"
        '!'     -> Just "Zba"
        '@'     -> Just "Zat"
        '#'     -> Just "Zha"
        '$'     -> Just "Zdl"
        '%'     -> Just "Zpl"
        '^'     -> Just "Zha"
        '&'     -> Just "Zam"
        '*'     -> Just "Zas"
        '~'     -> Just "Ztl"
        '-'     -> Just "Zmi"
        '+'     -> Just "Zpl"
        '='     -> Just "Zeq"
        '|'     -> Just "Zpi"
        '\\'    -> Just "Zbs"
        '/'     -> Just "Zfs"
        ':'     -> Just "Zco"
        '?'     -> Just "Zqn"
        '<'     -> Just "Zlt"
        '>'     -> Just "Zgt"
        '['     -> Just "Zbr"
        ']'     -> Just "Zke"
        '\''    -> Just "Zpm"
        '`'     -> Just "Zbt"
        _       -> Nothing

