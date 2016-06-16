
-- | Definition of Source Tetra Expressions.
module DDC.Source.Tetra.Exp
        ( -- * Binding
          Bind          (..)
        , Bound         (..)

          -- * Types

          -- ** Syntax
        , GTAnnot
        , GTBindVar,    GTBoundVar
        , GTBindCon,    GTBoundCon
        , GTPrim

        , Type,         GType  (..)
        , TyCon,        GTyCon (..)

        , SoCon         (..)
        , KiCon         (..)
        , TwCon         (..)
        , TcCon         (..)

        , TyConBind     (..)
        , TyConBound    (..)

        , pattern TApp2, pattern TApp3
        , pattern TApp4, pattern TApp5

        , pattern TVoid, pattern TUnit
        , pattern TFun
        , pattern TBot,  pattern TSum
        , pattern TForall
        , pattern TExists
        , pattern TPrim

          -- ** Primitives 
        , PrimType       (..)
        , PrimTyCon      (..)
        , PrimTyConTetra (..)

        , pattern KData, pattern KRegion, pattern KEffect
        , pattern TImpl
        , pattern TSusp
        , pattern TRead, pattern TWrite,  pattern TAlloc

        , pattern TBool
        , pattern TNat,  pattern TInt
        , pattern TSize, pattern TWord
        , pattern TFloat
        , pattern TTextLit


          -- * Terms
          -- ** Syntax
        , GXAnnot
        , GXBindVar,    GXBoundVar
        , GXBindCon,    GXBoundCon
        , GXPrim

        , BindVarMT,    GXBindVarMT (..)
        , Exp,          GExp        (..)
        , Lets,         GLets       (..)
        , Clause,       GClause     (..)
        , Alt,          GAlt        (..)
        , Pat,          GPat        (..)
        , GuardedExp,   GGuardedExp (..)
        , Guard,        GGuard      (..)
        , Cast,         GCast       (..)
        , Witness,      GWitness    (..)
        , WiCon,        GWiCon      (..)
        , DaCon (..)

        , DaConBind     (..)
        , DaConBound    (..)

          -- ** Primitives
        , PrimVal       (..)
        , PrimArith     (..)
        , OpVector      (..)
        , OpFun         (..)
        , OpError       (..)
        , PrimLit       (..)

        , pattern PTrue
        , pattern PFalse

        -- * Dictionaries
        , ShowLanguage
        , PrettyLanguage
        , NFDataLanguage)
where
import DDC.Source.Tetra.Exp.Source
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.NFData
import DDC.Source.Tetra.Pretty

