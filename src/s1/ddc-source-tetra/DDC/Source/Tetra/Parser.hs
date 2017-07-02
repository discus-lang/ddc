
-- | Parser for the Source Tetra language.
module DDC.Source.Tetra.Parser
        ( Parser

        -- * Modules
        , pModule

        -- * Expressions
        , pExp
        , pExpAppSP

        -- * Types
        , pType
        , pTypeApp
        , pTypeArgSP

        -- * Witnesses
        , pWitness
        , pWitnessApp
        , pWitnessAtom

        -- * Variables
        , pBindNameSP
        , pBoundNameSP,         pBoundName
        , pBoundIxSP
        , pBoundNameOpSP
        , pBoundNameOpVarSP

        -- * Constructors
        , pDaConBindName
        , pDaConBoundName,      pDaConBoundNameSP
        , pDaConBoundLit,       pDaConBoundLitSP
        , pPrimValSP

        -- * Raw Tokens
        , pTok)
where
import DDC.Source.Tetra.Parser.Module
import DDC.Source.Tetra.Parser.Witness
import DDC.Source.Tetra.Parser.Exp
import DDC.Source.Tetra.Parser.Base

