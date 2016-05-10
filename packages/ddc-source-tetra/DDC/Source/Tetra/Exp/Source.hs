{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Exp.Source
        ( Source        (..)

        -- * Binding
        , Bind          (..)
        , Bound         (..)

        -- * Types
        -- ** Abstract Syntax
        , GAnnot
        , GBindVar, GBoundVar
        , GBindCon, GBoundCon
        , GPrim

        -- *** Expressions
        , Type,  GType  (..)

        -- *** Constructors
        , TyCon, GTyCon (..)

        -- ** Syntactic Sugar
        , pattern TFun
        , pattern TUnit
        , pattern TVoid
        , pattern TForall
        , pattern TExists
        , pattern TPrim
        , pattern TData
        , pattern TRegion
        , pattern TEffect
        , pattern TKiFun
        , pattern TDaImpl
        , pattern TDaFun

        -- ** Type Constructors
        , TyConPrim      (..)
        , SoCon          (..)
        , KiCon          (..)
        , TwCon          (..)
        , TcCon          (..)
        , PrimTyCon      (..)
        , PrimTyConTetra (..))
where
import DDC.Source.Tetra.Prim
import DDC.Type.Exp.Generic.Exp         as T
import DDC.Type.Exp.TyCon               as T
import DDC.Data.SourcePos
import Data.Text                                (Text)

-- | Type index for Source Tetra Language.
data Source     
        = Source
        deriving Show


-- | Binding occurrence of a variable.
data Bind
        = BNone
        | BAnon
        | BName !Text
        deriving Show


-- | Bound occurrence of a variable.
data Bound 
        = UIx   !Int
        | UName !Text
        deriving Show

type Type       = GType  Source
type TyCon      = GTyCon Source

type instance T.GAnnot    Source  = SourcePos
type instance T.GBindVar  Source  = Bind
type instance T.GBoundVar Source  = Bound
type instance T.GBindCon  Source  = Text
type instance T.GBoundCon Source  = Text
type instance T.GPrim     Source  = TyConPrim


-------------------------------------------------------------------------------
-- | Primitive type constructors.
data TyConPrim
        -- | Sort constructors.
        = TyConPrimSoCon SoCon

        -- | Kind constructors.
        | TyConPrimKiCon KiCon

        -- | Witness type constructors.
        | TyConPrimTwCon TwCon

        -- | Other type constructors at the spec level.
        | TyConPrimTcCon TcCon

        -- | Machine type constructors.
        | TyConPrimMach  PrimTyCon

        -- | Tetra type constructors.
        | TyConPrimTetra PrimTyConTetra
        deriving Show


-------------------------------------------------------------------------------
-- | Annotated kind function constructor.
pattern TKiFun  = TCon (TyConPrim (TyConPrimKiCon KiConFun))

-- | Annotated implication constructor.
pattern TDaImpl = TCon (TyConPrim (TyConPrimTwCon TwConImpl))

-- | Annotated function constructor.
pattern TDaFun  = TCon (TyConPrim (TyConPrimTcCon TcConFun))

-- | Annotated effect type constructor.
pattern TData   = TCon (TyConPrim (TyConPrimKiCon KiConData))

-- | Annotated effect type constructor.
pattern TRegion = TCon (TyConPrim (TyConPrimKiCon KiConRegion))

-- | Annotated effect type constructor.
pattern TEffect = TCon (TyConPrim (TyConPrimKiCon KiConEffect))


