
module DDC.Core.Exp.Generic
        ( ---------------------------------------
          -- * Abstract Syntax
          -- ** Expressions
          GAnnot
        , GBind
        , GBound
        , GPrim

        , GExp          (..)
        , GParam        (..)
        , GArg          (..)
        , GLets         (..)
        , GAlt          (..)
        , GPat          (..)
        , GCast         (..)
        , GWitness      (..)
        , GWiCon        (..)

        , pattern XLAM
        , pattern XLam

          ---------------------------------------
          -- * Predicates
        , module DDC.Type.Exp.Simple.Predicates

          -- ** Atoms
        , isXVar,  isXCon
        , isAtomX, isAtomR, isAtomW

          -- ** Abstractions
        , isXAbs, isXLAM, isXLam

          -- ** Applications
        , isXApp

          -- ** Let bindings
        , isXLet

          -- ** Patterns
        , isPDefault

          ---------------------------------------
          -- * Compounds
        , module DDC.Type.Exp.Simple.Compounds

          -- ** Abstractions
        , makeXAbs,     takeXAbs
        , makeXLAMs,    takeXLAMs
        , makeXLams,    takeXLams

          -- ** Applications
        , makeXApps,    takeXApps,      splitXApps
        , takeXConApps
        , takeXPrimApps

          -- ** Data Constructors
        , dcUnit
        , takeNameOfDaConPrim
        , takeNameOfDaConBound
        , takeBaseCtorNameOfDaCon

          ---------------------------------------
          -- * Dictionaries
        , ShowLanguage)
where
import DDC.Core.Exp.Generic.Exp
import DDC.Core.Exp.Generic.Predicates
import DDC.Core.Exp.Generic.Compounds
import DDC.Core.Exp.Generic.Pretty              ()
import DDC.Type.Exp.Simple.Compounds
import DDC.Type.Exp.Simple.Predicates


