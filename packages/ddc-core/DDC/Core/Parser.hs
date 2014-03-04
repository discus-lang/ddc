-- | Core language parser.
module DDC.Core.Parser
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
import DDC.Core.Parser.Param
