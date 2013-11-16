
module DDCI.Core.Mode
        ( Mode(..)
        , readMode
        , suppressConfigOfModes)
where
import qualified DDC.Core.Transform.Suppress    as Suppress
import qualified Data.Set                       as Set
import Data.Set                                 (Set)

-- | DDCI mode flags.
data Mode
        -- | Render expressions displayed to user using indenting.
        =  Indent

        -- | Suppress import lists when printing modules
        |  SuppressImports

        -- | Suppress type annotations on let-bindings.
        |  SuppressLetTypes

        -- | Display type checker trace with check commands.
        |  TraceCheck

        -- | Display the expression at each step in the evaluation.
        |  TraceEval

        -- | Display the store state at each step in the evaluation.
        |  TraceStore

        -- | Display information about each transformation step
        |  TraceTrans

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
        "Indent"                -> Just Indent
        "SuppressImports"       -> Just SuppressImports
        "SuppressLetTypes"      -> Just SuppressLetTypes
        "TraceCheck"            -> Just TraceCheck
        "TraceEval"             -> Just TraceEval
        "TraceStore"            -> Just TraceStore
        "TraceTrans"            -> Just TraceTrans
        "SaltPrelude"           -> Just SaltPrelude
        "Dump"                  -> Just Dump
        "TaintAvoidTypeChecks"  -> Just TaintAvoidTypeChecks
        _                       -> Nothing


-- | Build a suppress config from any appropriate mode flags.
suppressConfigOfModes :: Set Mode -> Suppress.Config
suppressConfigOfModes mm'
 = go (Set.toList mm')
 where go mm
        = case mm of
                SuppressLetTypes : moar
                 -> (go moar) { Suppress.configLetTypes = True }
                _ : moar        -> go moar
                []              -> Suppress.configZero
        
