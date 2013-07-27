
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
import DDC.Core.Parser.Base
import DDC.Core.Parser.Context
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Type
import DDC.Source.Tetra.Parser.Exp
import DDC.Source.Tetra.Parser.Module

        
