
-- | Definition of Source Discus Abstract Syntax,
--   and utilities for working with it.
module DDC.Source.Discus.Exp
        ( -- * Binding
          Name
        , Bind          (..)
        , Bound         (..)
        , takeBoundOfBind

          -----------------------------
          -- ** Annotations
        , SourcePos

          -----------------------------
          -- * Types
          -- ** Syntax
          -- *** Expressions
        , Type,         GType  (..)

          -- *** TyCons
        , TyCon,        GTyCon (..)
        , TyConBind     (..)
        , TyConBound    (..)

          -----------------------------
          -- ** Type Constructors
        , SoCon         (..)
        , KiCon         (..)
        , TwCon         (..)
        , TcCon         (..)

          -----------------------------
          -- ** Type Primitives
        , TyConPrim     (..)
        , PrimTyCon     (..)
        , TyConDiscus   (..)

          -----------------------------
          -- ** Pattern Synonyms
        , pattern TApp2, pattern TApp3
        , pattern TApp4, pattern TApp5

        , pattern TVoid, pattern TUnit
        , pattern TBot,  pattern TUnion
        , pattern TPrim
        , pattern TFunExplicit
        , pattern TFunImplicit

        , pattern KData, pattern KRegion, pattern KEffect
        , pattern TImpl
        , pattern TSusp
        , pattern TRead, pattern TWrite,  pattern TAlloc

        , pattern TBool
        , pattern TNat,  pattern TInt
        , pattern TSize, pattern TWord
        , pattern TFloat
        , pattern TTextLit

        , pattern TVector
        , pattern TFunValue

          -----------------------------
          -- ** Predicates
        , isAtomT

          -----------------------------
          -- ** Compounds
        , -- *** Destructors
          takeTCon
        , takeTVar
        , takeTAbs
        , takeTApp

          -- *** Type Applications
        , makeTApps,    takeTApps

          -- *** Function Types
        , makeTFun,     makeTFuns,      makeTFuns',     (~>)
        , takeTFun,     takeTFuns,      takeTFuns'

          -- *** Forall Types
        , makeTForall,  makeTForalls
        , takeTForall

          -- *** Exists Types
        , makeTExists,  takeTExists

          -- *** Union types
        , takeTUnion
        , makeTUnions,  takeTUnions
        , splitTUnionsOfKind
        , makeTBot

          -------------------------------------------------
          -- * Terms
          -- ** Syntax
        , BindVarMT,    GXBindVarMT (..)

          -- *** Expressions
        , Exp,          GExp        (..)

          -- *** Parameters
        , ParamSort     (..)
        , Param,        GParam      (..)

          -- *** Arguments
        , Arg,          GArg        (..)

          -- *** Let-binding
        , Lets,         GLets       (..)

          -- *** Capabilities
        , Caps,         GCaps       (..)

          -- *** Clauses
        , Clause,       GClause     (..)

          -- *** Patterns
        , Pat,          GPat        (..)

          -- *** Guards
        , Guard,        GGuard      (..)

          -- *** Guarded Expressions
        , GuardedExp,   GGuardedExp (..)

          -- *** Case Alternatives
        , AltCase,      GAltCase    (..)
        , AltMatch,     GAltMatch   (..)

          -- *** Casts
        , Cast,         GCast       (..)

          -- *** Witnesses
        , Witness,      GWitness    (..)

          -- *** Witness Constructors
        , WiCon,        GWiCon      (..)

          -- *** Data Constructors
        , DaCon (..)
        , DaConBind     (..)
        , DaConBound    (..)

          -----------------------------
          -- ** Term Primitives
        , PrimVal       (..)
        , PrimArith     (..)
        , PrimCast      (..)
        , PrimLit       (..)
        , OpVector      (..)
        , OpFun         (..)
        , OpError       (..)
        , Literal       (..)
        , Text

          -----------------------------
          -- ** Pattern Synonyms
        , pattern PTrue
        , pattern PFalse

          -----------------------------
          -- ** Predicates
          -- *** Atoms
        , isXVar,       isXCon
        , isAtomX,      isAtomW

          -- *** Abstractions
        , isXAbs

          -- *** Applications
        , isXApp

          -- *** Let bindings
        , isXLet

          -- *** Patterns
        , isPDefault

          -----------------------------
          -- ** Compounds
        , takeAnnotOfExp

          -- *** Binds
        , bindOfBindMT
        , takeTypeOfBindMT

          -- *** Applications
        , makeXApps
        , makeXAppsWithAnnots
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXAppsWithAnnots
        , takeXConApps

          -- *** Clauses
        , bindOfClause

          -- *** Casts
        , pattern XRun
        , pattern XBox

          -- *** Data Constructors
        , dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon

          -- *** Patterns
        , bindsOfPat

          -- *** Witnesses
        , wApp
        , wApps
        , takeWAppsAsList
        , takePrimWiConApps

          -- *** Primitives
        , primLitOfLiteral
        , makeXErrorDefault

        -------------------------------------------------
        -- * Data Declarations
        , DataDef       (..)
        , DataCtor      (..)
        , typeOfDataCtor)
where

import DDC.Source.Discus.Exp.Type.Compounds
import DDC.Source.Discus.Exp.Type.NFData        ()
import DDC.Source.Discus.Exp.Type.Pretty        ()
import DDC.Source.Discus.Exp.Type.Base

import DDC.Source.Discus.Exp.Term.Predicates
import DDC.Source.Discus.Exp.Term.Compounds
import DDC.Source.Discus.Exp.Term.NFData        ()
import DDC.Source.Discus.Exp.Term.Pretty        ()
import DDC.Source.Discus.Exp.Term.Base

import DDC.Source.Discus.Exp.DataDef

import DDC.Type.Exp.TyCon               as T

import DDC.Data.SourcePos


-- Type AST -------------------------------------------------------------------
type Type       = GType       SourcePos
type TyCon      = GTyCon      SourcePos

-- Term AST -------------------------------------------------------------------
type BindVarMT  = GXBindVarMT SourcePos
type Exp        = GExp        SourcePos
type Param      = GParam      SourcePos
type Arg        = GArg        SourcePos
type Lets       = GLets       SourcePos
type Caps       = GCaps       SourcePos
type Clause     = GClause     SourcePos
type Pat        = GPat        SourcePos
type Guard      = GGuard      SourcePos
type GuardedExp = GGuardedExp SourcePos
type AltCase    = GAltCase    SourcePos
type AltMatch   = GAltMatch   SourcePos
type Cast       = GCast       SourcePos
type Witness    = GWitness    SourcePos
type WiCon      = GWiCon      SourcePos

