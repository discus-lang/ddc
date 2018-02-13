
-- | Parser for the Source Discus language.
module DDC.Source.Discus.Parser
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
import DDC.Source.Discus.Parser.Module
import DDC.Source.Discus.Parser.Witness
import DDC.Source.Discus.Parser.Exp
import DDC.Source.Discus.Parser.Base

