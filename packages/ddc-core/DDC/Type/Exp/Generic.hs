
module DDC.Type.Exp.Generic
        ( -- * Abstract Syntax
          -- ** Type Families
          GTAnnot
        , GTBindVar, GTBoundVar
        , GTBindCon, GTBoundCon
        , GTPrim

          -- ** Core Syntax
        , GType (..), GTyCon (..)

          -- ** Syntactic Sugar
        , pattern TUnit
        , pattern TVoid
        , pattern TBot
        , pattern TPrim
        , pattern TFunExplicit
        , pattern TFunImplicit 
        
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
        , ShowGType)
where
import DDC.Type.Exp.Generic.Exp
import DDC.Type.Exp.Generic.Binding
import DDC.Type.Exp.Generic.Compounds

