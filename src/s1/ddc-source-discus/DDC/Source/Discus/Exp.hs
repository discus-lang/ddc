{-# LANGUAGE TypeFamilies #-}

-- | Definition of Source Discus Abstract Syntax,
--   and utilities for working with it.
module DDC.Source.Discus.Exp
        ( -- * Binding
          Name
        , Bind          (..)
        , Bound         (..)
        , takeBoundOfBind

          -------------------------------------------------
          -- * Types

          -----------------------------
          -- ** Syntax
          -- *** Expressions
        , Type,         GType  (..)

          -- *** TyCons
        , TyCon,        GTyCon (..)
        , TyConBind     (..)
        , TyConBound    (..)

          -----------------------------
          -- ** Type Generics
        , Source        (..)
        , GTAnnot
        , GTBindVar,    GTBoundVar
        , GTBindCon,    GTBoundCon
        , GTPrim

          -----------------------------
          -- ** Type Constructors
        , SoCon         (..)
        , KiCon         (..)
        , TwCon         (..)
        , TcCon         (..)

          -----------------------------
          -- ** Type Primitives
        , PrimType       (..)
        , PrimTyCon      (..)
        , PrimTyConDiscus (..)

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
        , Annot,        GXAnnot
        , BindVar,      GXBindVar
        , BindVarMT,    GXBindVarMT (..)
        , BoundVar,     GXBoundVar
        , BindCon,      GXBindCon
        , BoundCon,     GXBoundCon
        , Frag,         GXFrag

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

          -- ** Primitives
        , Prim          (..)

          -----------------------------
          -- ** Term Primitives
        , PrimVal       (..)
        , PrimArith     (..)
        , OpVector      (..)
        , OpFun         (..)
        , OpError       (..)
        , PrimLit       (..)

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
        , takeXFragApps

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

        -------------------------------------------------
        -- * Data Declarations
        , DataDef       (..)
        , DataCtor      (..)
        , typeOfDataCtor

        -------------------------------------------------
        -- * Dictionaries
        , ShowLanguage
        , NFDataLanguage)
where

import DDC.Source.Discus.Exp.Term.Predicates
import DDC.Source.Discus.Exp.Term.Compounds
import DDC.Source.Discus.Exp.Term.NFData
import DDC.Source.Discus.Exp.Term.Base

import DDC.Source.Discus.Exp.Type.Compounds

import DDC.Source.Discus.Exp.Bind
import DDC.Source.Discus.Exp.DataDef

import DDC.Source.Discus.Prim


import DDC.Type.Exp.TyCon               as T

import DDC.Data.SourcePos
import DDC.Data.Pretty


-- Language -------------------------------------------------------------------
-- | Type index for Source Discus Language.
data Source
        = Source
        deriving Show

instance Pretty Source where
 ppr ss = text (show ss)


instance HasAnonBind Source where
 isAnon _ BAnon = True
 isAnon _ _     = False


instance Anon Source where
 withBindings Source n f
  = let bs      = replicate n BAnon
        us      = reverse [UIx i | i <- [0..(n - 1)]]
    in  f bs us


-- Type AST -------------------------------------------------------------------
type Type       = GType  Source
type TyCon      = GTyCon Source

type instance GTAnnot    Source = SourcePos
type instance GTBindVar  Source = Bind
type instance GTBoundVar Source = Bound
type instance GTBindCon  Source = TyConBind
type instance GTBoundCon Source = TyConBound
type instance GTPrim     Source = PrimType


-- Term AST -------------------------------------------------------------------
type Annot      = GXAnnot     Source
type BindVar    = GXBindVar   Source
type BindVarMT  = GXBindVarMT Source
type BoundVar   = GXBoundVar  Source
type BindCon    = GXBoundCon  Source
type BoundCon   = GXBoundCon  Source
type Frag       = GXFrag      Source
type Exp        = GExp        Source
type Param      = GParam      Source
type Arg        = GArg        Source
type Lets       = GLets       Source
type Caps       = GCaps       Source
type Clause     = GClause     Source
type Pat        = GPat        Source
type Guard      = GGuard      Source
type GuardedExp = GGuardedExp Source
type AltCase    = GAltCase    Source
type AltMatch   = GAltMatch   Source
type Cast       = GCast       Source
type Witness    = GWitness    Source
type WiCon      = GWiCon      Source

type instance GXAnnot    Source = SourcePos
type instance GXBindVar  Source = Bind
type instance GXBoundVar Source = Bound
type instance GXBindCon  Source = DaConBind
type instance GXBoundCon Source = DaConBound
type instance GXFrag     Source = PrimVal




