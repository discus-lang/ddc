
module DDC.Type.Exp.Generic
        ( -- * Abstract Syntax
          -- ** Type Families
          GAnnot, GBind, GBound, GPrim

          -- ** Core Syntax
        , GType (..), GCon (..)

          -- ** Syntactic Sugar
        , pattern TFun
        , pattern TUnit
        , pattern TVoid
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

