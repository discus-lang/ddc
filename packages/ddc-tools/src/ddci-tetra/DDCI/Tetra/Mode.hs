
module DDCI.Tetra.Mode
        ( Mode (..)
        , readMode)
where


-- | ddci-tetra mode flags.
data Mode
        -- | Dump all intermediate versions of the code during compilation.
        = Dump
        deriving (Eq, Ord, Show)


-- Parse a mode from a string.
readMode :: String -> Maybe Mode
readMode str
 = case str of
        "Dump"          -> Just Dump
        _               -> Nothing


