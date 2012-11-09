-- | Core language parser.
module DDC.Core.Parser
        ( Parser

        -- * Modules
        , pModule

          -- * Expressions
        , pExp
        , pExpApp
        , pExpAtom

          -- * Types
        , pType
        , pTypeApp
        , pTypeAtom

          -- * Witnesses
        , pWitness
        , pWitnessApp
        , pWitnessAtom

          -- * Constructors
        , pCon
        , pLit

          -- * Variables
        , pBinder
        , pIndex
        , pVar
        , pName

          -- * Raw Tokens
        , pTok
        , pTokAs)

where
import DDC.Core.Parser.Base
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Type
import DDC.Core.Parser.Exp
import DDC.Core.Parser.Module


