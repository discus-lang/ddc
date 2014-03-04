
module DDC.Source.Tetra.Parser
        ( Parser
        , Context       (..)

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
import DDC.Source.Tetra.Parser.Exp
import DDC.Source.Tetra.Parser.Module

import DDC.Core.Parser
        ( Parser
        , Context       (..)
        , pWitness
        , pWitnessApp
        , pWitnessAtom
        , pVar
        , pCon
        , pName
        , pBinder
        , pIndex        
        , pLit
        , pTok, pTokAs)
        
