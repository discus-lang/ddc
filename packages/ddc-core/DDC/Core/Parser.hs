-- | Core language parser.
module DDC.Core.Parser
        ( Parser

        -- * Modules
        , pModule

          -- * Expressions
        , pExp
        , pExpAtom

          -- * Types
        , pType
        , pTypeAtom

          -- * Witnesses
        , pWitness
        , pWitnessAtom)
where
import DDC.Core.Parser.Base
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Type
import DDC.Core.Parser.Exp
import DDC.Core.Parser.Module


