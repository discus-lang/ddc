-- | Core language parser.
module DDC.Core.Parser
        ( Parser
        , Context       (..)
        , contextOfProfile

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
        , pCon, pConSP
        , pLit, pLitSP

          -- * Variables
        , pIndex,       pIndexSP
        , pVar,         pVarSP
        , pBinder
        , pName
        
          -- * Infix operators
        , pOpSP
        , pOpVarSP

          -- * Raw Tokens
        , pTok,         pTokSP
        , pTokAs)

where
import DDC.Core.Parser.Base
import DDC.Core.Parser.Context
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Type
import DDC.Core.Parser.Exp
import DDC.Core.Parser.Module

