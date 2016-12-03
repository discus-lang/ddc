
-- | Definition of Source Tetra Abstract Syntax,
--   and utilities for working with it.
module DDC.Source.Tetra.Exp
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
        , PrimTyConTetra (..)

          -----------------------------
          -- ** Pattern Synonyms
        , pattern TApp2, pattern TApp3
        , pattern TApp4, pattern TApp5

        , pattern TVoid, pattern TUnit
        , pattern TFun
        , pattern TBot,  pattern TUnion
        , pattern TPrim

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

          -- *** Let-binding
        , Lets,         GLets       (..)

          -- *** Clauses
        , Clause,       GClause     (..)

          -- *** Parameters
        , Param,        GParam      (..)
        , ParamSort     (..)

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
--        , isXLAM, isXLam
--        , isLambdaX

          -- *** Applications
        , isXApp

          -- *** Let bindings
        , isXLet

          -- *** Types and Witnesses
        , isXType
        , isXWitness

          -- *** Patterns
        , isPDefault

          -----------------------------
          -- ** Compounds
        , takeAnnotOfExp

          -- *** Binds
        , bindOfBindMT
        , takeTypeOfBindMT

          -- *** Lambdas
{-
        , makeXLAMs
        , makeXLams
        , makeXLamFlags
        , takeXLAMs
        , takeXLams
        , takeXLamFlags
-}
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
        , takeXWitness
        , takeWAppsAsList
        , takePrimWiConApps

        -------------------------------------------------
        -- * Dictionaries
        , ShowLanguage
        , PrettyLanguage
        , NFDataLanguage)
where
import DDC.Source.Tetra.Exp.Bind
import DDC.Source.Tetra.Exp.Source
import DDC.Source.Tetra.Exp.Predicates
import DDC.Source.Tetra.Exp.Compounds
import DDC.Source.Tetra.Exp.NFData
import DDC.Source.Tetra.Pretty
import DDC.Type.Exp.Generic.Compounds

