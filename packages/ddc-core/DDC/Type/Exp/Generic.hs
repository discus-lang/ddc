
module DDC.Type.Exp.Generic
        ( -- * Abstract Syntax
          -- ** Type Families
          GAnnot
        , GBindVar, GBoundVar
        , GBindCon, GBoundCon
        , GPrim

          -- ** Core Syntax
        , GType (..), GTyCon (..)

          -- ** Syntactic Sugar
        , pattern TFun
        , pattern TUnit
        , pattern TVoid
        , pattern TBot
        , pattern TForall
        , pattern TExists
        , pattern TPrim

          -- * Compounds
          -- ** Type Applications
        , makeTApps,    takeTApps

          -- ** Function Types
        , makeTFun,     makeTFuns
        , takeTFun,     takeTFuns,      takeTFuns'

          -- ** Forall Types
        , makeTForall,  takeTForall

          -- ** Exists Types
        , makeTExists,  takeTExists

          -- * Type Classes
        , Binding       (..)
        , Anon          (..)
        , ShowLanguage)
where
import DDC.Type.Exp.Generic.Exp
import DDC.Type.Exp.Generic.Binding
import DDC.Type.Exp.Generic.Compounds

