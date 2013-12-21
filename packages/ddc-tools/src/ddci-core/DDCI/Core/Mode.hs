
module DDCI.Core.Mode
        ( Mode(..)
        , readMode)
where


-- | ddci-core mode flags.
data Mode
        -- | Use type synthesis / bidirectional type checking
        --   when loading source code.
        =  Synth

        -- | Render expressions displayed to user using indenting.
        |  Indent

        -- Tracing --------------------
        -- | Display type checker trace with check commands.
        |  TraceCheck

        -- | Display the expression at each step in the evaluation.
        |  TraceEval

        -- | Display the store state at each step in the evaluation.
        |  TraceStore

        -- | Display information about each transformation step
        |  TraceTrans

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

        ------------------------------
        -- | When pretty printing Salt modules as C code,
        --  include the #includes etc needed for compilation.
        |  SaltPrelude

        -- | Dump all intermediate versions of the code during compilation.
        |  Dump
	
        -- | Avoid type checking if possible.
        --   Helpful when debugging program transformations to see invalid
        --   code instead of an error message.
        |  TaintAvoidTypeChecks
        deriving (Eq, Ord, Show)


-- | Parse a mode from a string.
readMode :: String -> Maybe Mode
readMode str
 = case str of
        "Synth"                 -> Just Synth
        "Indent"                -> Just Indent
        "TraceCheck"            -> Just TraceCheck
        "TraceEval"             -> Just TraceEval
        "TraceStore"            -> Just TraceStore
        "TraceTrans"            -> Just TraceTrans
        "PrettyUseLetCase"      -> Just PrettyUseLetCase
        "PrettyVarTypes"        -> Just PrettyVarTypes
        "PrettyConTypes"        -> Just PrettyConTypes
        "SuppressImports"       -> Just SuppressImports
        "SuppressExports"       -> Just SuppressExports
        "SuppressLetTypes"      -> Just SuppressLetTypes
        "SaltPrelude"           -> Just SaltPrelude
        "Dump"                  -> Just Dump
        "TaintAvoidTypeChecks"  -> Just TaintAvoidTypeChecks
        _                       -> Nothing

