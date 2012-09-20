
module DDCI.Core.Mode
        ( Mode(..)
        , readMode)
where


-- | DDCI mode flags.
data Mode
        -- | Display the expression at each step in the evaluation.
        =  TraceEval

        -- | Display the store state at each step in the evaluation.
        |  TraceStore

        -- | Render expressions displayed to user using indenting.
        |  Indent

        -- | Suppress import lists when printing modules
        |  SuppressImports

        -- | When pretty printing Salt modules as C code,
        --  include the #includes etc needed for compilation.
        |  SaltPrelude

        -- | Dump all intermediate versions of the code during compilation.
        |  Dump

	-- | Display information about each transformation step
	|  TraceTrans
        deriving (Eq, Ord, Show)


-- | Parse a mode from a string.
readMode :: String -> Maybe Mode
readMode str
 = case str of
        "TraceEval"             -> Just TraceEval
        "TraceStore"            -> Just TraceStore
        "Indent"                -> Just Indent
        "SuppressImports"       -> Just SuppressImports
        "SaltPrelude"           -> Just SaltPrelude
        "Dump"                  -> Just Dump
        "TraceTrans"            -> Just TraceTrans
        _                       -> Nothing

