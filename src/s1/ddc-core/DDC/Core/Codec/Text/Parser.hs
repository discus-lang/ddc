-- | Core language parser.
module DDC.Core.Codec.Text.Parser
        ( Parser
        , Context       (..)
        , contextOfProfile

          -- * Types
        , pType
        , pTypeApp
        , pTypeAtom

        -- * Modules
        , pModule
        , pModuleName

        -- * Expressions
        , pExp
        , pExpApp
        , pExpAtom

        -- * Function Parameters
        , ParamSpec(..)
        , funTypeOfParams
        , expOfParams
        , pBindParamSpecAnnot
        , pBindParamSpec

          -- * Witnesses
        , pWitness
        , pWitnessApp
        , pWitnessAtom

          -- * Constructors
        , pCon,         pConSP
        , pLit,         pLitSP

          -- * Variables
        , pIndex,       pIndexSP
        , pVar,         pVarSP
        , pBinder
        , pName

          -- * Infix operators
        , pOpSP
        , pOpVarSP

          -- * Raw Tokens
        , pSym,         pKey
        , pTok,         pTokSP
        , pTokAs)

where
import DDC.Core.Codec.Text.Parser.Base
import DDC.Core.Codec.Text.Parser.Context
import DDC.Core.Codec.Text.Parser.Witness
import DDC.Core.Codec.Text.Parser.Type
import DDC.Core.Codec.Text.Parser.Exp
import DDC.Core.Codec.Text.Parser.Module
import DDC.Core.Codec.Text.Parser.Param
