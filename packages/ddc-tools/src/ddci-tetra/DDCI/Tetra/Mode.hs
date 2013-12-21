
module DDCI.Tetra.Mode
        ( Mode (..)
        , readMode)
where


-- | ddci-tetra mode flags.
data Mode
        -- | Dump all intermediate versions of the code during compilation.
        = Dump

        -- Pretty Printer Config ------
        -- There is one mode here for each field in Driver.Config.ConfigPretty

        -- | Use 'letcase' when pretty printing core modules.
        |  PrettyUseLetCase

        -- | Display types on primitive variables.
        |  PrettyVarTypes

        -- | Display types on primitive constructors.
        |  PrettyConTypes

        -- | Suppress import lists when printing modules.
        |  SuppressImports

        -- | Suppress export lists when printing modules.
        |  SuppressExports

        -- | Suppress type annotations on let-bindings.
        |  SuppressLetTypes

        deriving (Eq, Ord, Show)


-- Parse a mode from a string.
readMode :: String -> Maybe Mode
readMode str
 = case str of
        "Dump"                  -> Just Dump
        "PrettyUseLetCase"      -> Just PrettyUseLetCase
        "PrettyVarTypes"        -> Just PrettyVarTypes
        "PrettyConTypes"        -> Just PrettyConTypes
        "SuppressImports"       -> Just SuppressImports
        "SuppressExports"       -> Just SuppressExports
        "SuppressLetTypes"      -> Just SuppressLetTypes
        _                       -> Nothing


