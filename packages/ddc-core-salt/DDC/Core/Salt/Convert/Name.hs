
module DDC.Core.Salt.Convert.Name
        ( sanitizeName
        , sanitizeGlobal
        , seaNameOfSuper
        , seaNameOfLocal)
where
import DDC.Core.Salt.Name
import DDC.Core.Module
import DDC.Base.Pretty
import Data.Maybe



-- | Like 'sanitizeGlobal' but indicate that the name is going to be visible
--   globally.
sanitizeGlobal :: String -> String
sanitizeGlobal = sanitizeName


-- | Convert the Salt name of a supercombinator to a name we can use when
--   defining the C function.
seaNameOfSuper 
        :: Maybe (ImportSource Name)    -- ^ How the super is imported
        -> Maybe (ExportSource Name)    -- ^ How the super is exported
        -> Name                         -- ^ Name of the super.
        -> Maybe Doc

seaNameOfSuper mImport mExport nm

        -- Super is defined in this module and not exported.
        | Nothing                               <- mImport
        , Nothing                               <- mExport
        , Just str                              <- takeNameVar nm
        = Just $ text $ {- "_DDC_" ++ -} sanitizeName str

        -- Super is defined in this module and exported to C land.
        | Nothing                               <- mImport
        , Just _                                <- mExport
        , Just str                              <- takeNameVar nm
        = Just $ text $ sanitizeName str

        -- Super is imported from another module and not exported.
        | Just (ImportSourceModule _ _ _)       <- mImport
        , Nothing                               <- mExport
        , Just str                              <- takeNameVar nm
        = Just $ text $ {- "_DDC_" ++ -} sanitizeName str
        
        -- Super is imported from C-land and not exported.
        | Just (ImportSourceSea strSea _)       <- mImport
        , Nothing                               <- mExport
        = Just $ text strSea

        -- ISSUE #320: Handle all the import/export combinations.
        --
        -- We don't handle the other cases because we would need to
        -- produce a wrapper to conver the names.
        | otherwise
        = Nothing


-- | Convert the Salt name of a local variable to a name we can use in the
--   body of a C function.
seaNameOfLocal :: Name -> Maybe Doc
seaNameOfLocal nn
 = case takeNameVar nn of
        Just str        -> Just $ text $ "_" ++ sanitizeGlobal str
        _               -> Nothing


-- Sanitize ---------------------------------------------------------------------------------------
-- | Rewrite a name to make it safe to export as an external C symbol.
--
--   Names containing unfriendly characters like '&' are prefixed with '_sym_'
--   and the '&' is replaced by 'ZAn'. Literal 'Z's such a name are doubled to 'ZZ'.
--
sanitizeName :: String -> String
sanitizeName str
 = let  hasSymbols      = any isJust $ map convertSymbol str
   in   if hasSymbols
         then "_sym_" ++ concatMap rewriteChar str
         else str


-- | Get the encoded version of a character.
rewriteChar :: Char -> String
rewriteChar c
        | Just str <- convertSymbol c      = "Z" ++ str
        | 'Z'      <- c                    = "ZZ"
        | otherwise                        = [c]


-- | Convert symbols to their sanitized form.
convertSymbol :: Char -> Maybe String
convertSymbol c
 = case c of
        '!'     -> Just "Bg"
        '@'     -> Just "At"
        '#'     -> Just "Hs"
        '$'     -> Just "Dl"
        '%'     -> Just "Pc"
        '^'     -> Just "Ht"
        '&'     -> Just "An"
        '*'     -> Just "St"
        '~'     -> Just "Tl"
        '-'     -> Just "Ms"
        '+'     -> Just "Ps"
        '='     -> Just "Eq"
        '|'     -> Just "Pp"
        '\\'    -> Just "Bs"
        '/'     -> Just "Fs"
        ':'     -> Just "Cl"
        '.'     -> Just "Dt"
        '?'     -> Just "Qm"
        '<'     -> Just "Lt"
        '>'     -> Just "Gt"
        '['     -> Just "Br"
        ']'     -> Just "Kt"
        '\''    -> Just "Pm"
        '`'     -> Just "Bt"
        _       -> Nothing

